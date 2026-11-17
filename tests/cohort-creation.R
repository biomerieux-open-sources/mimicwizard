#!/usr/bin/env Rscript

library(DBI)
library(dplyr)
library(dbplyr)
library(shiny)
library(tidyr)

source("R/helpers/database.R")
source("R/helpers/logical.R")
source("R/helpers/cohort_config.R")
source("R/patient_events_server.R")
source("R/modules/event_searchbar_server.R")
source("R/modules/cohort_outcomes_explorer_server.R")
source("R/cohort_creation_server.R")

dbname <- Sys.getenv("COHORT_TEST_DB")
if (!identical(dbname, "mimicwizard_cohort_test")) {
  stop("COHORT_TEST_DB must be mimicwizard_cohort_test, a dedicated disposable database.")
}
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = Sys.getenv("PGHOST", "localhost"),
  port = as.integer(Sys.getenv("PGPORT", "5432")),
  user = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)
dbBegin(con)

tryCatch({
  existing <- dbGetQuery(con, paste(
    "SELECT nspname FROM pg_namespace WHERE nspname IN",
    "('mimiciv_icu', 'mimiciv_hosp', 'mimiciv_derived')"
  ))
  if (nrow(existing)) stop("Test schemas already exist; refusing to alter this database.")
  for (name in c("cohort", "d_cohorts", "demographics")) {
    if (dbExistsTable(con, Id(schema = "public", table = name))) {
      stop("public.", name, " already exists; refusing to alter this database.")
    }
  }

  statements <- c(
    "CREATE SCHEMA mimiciv_icu",
    "CREATE SCHEMA mimiciv_hosp",
    "CREATE SCHEMA mimiciv_derived",
    paste("CREATE TABLE public.cohort (cohort_id integer, subject_id integer,",
          "hadm_id integer, stay_id integer)"),
    paste("INSERT INTO public.cohort VALUES (1, 1, 11, 101), (2, 2, 22, 201),",
          "(3, 1, 11, 101), (3, 1, 11, 101), (3, 3, 33, NULL)"),
    "CREATE TABLE public.d_cohorts (cohort_id integer, cohort_name text)",
    "INSERT INTO public.d_cohorts VALUES (1, 'one'), (2, 'two'), (3, 'three')",
    paste("CREATE TABLE public.demographics (subject_id integer, hadm_id integer,",
          "stay_id integer, label text, value text, valueuom text, itemid integer)"),
    paste("INSERT INTO public.demographics VALUES",
          "(1, 11, 101, 'age', '50', 'y.o.', 1),",
          "(1, 11, 101, 'gender', 'F', '', 2),",
          "(2, 22, 201, 'age', '70', 'y.o.', 1)"),
    paste("CREATE TABLE mimiciv_icu.icustays (subject_id integer, hadm_id integer,",
          "stay_id integer, intime timestamp, outtime timestamp)"),
    paste("CREATE TABLE mimiciv_icu.chartevents (subject_id integer, hadm_id integer,",
          "stay_id integer, itemid integer, charttime timestamp, valuenum numeric)"),
    paste("CREATE TABLE mimiciv_hosp.labevents (subject_id integer, hadm_id integer,",
          "itemid integer, charttime timestamp, valuenum numeric)"),
    paste("CREATE TABLE mimiciv_derived.icustay_hourly",
          "(stay_id integer, hr integer, endtime timestamp)"),
    paste("CREATE TABLE mimiciv_hosp.diagnoses_icd",
          "(subject_id integer, hadm_id integer, seq_num integer, icd_code text, icd_version integer)"),
    paste("CREATE TABLE mimiciv_hosp.d_icd_diagnoses",
          "(icd_code text, icd_version integer, long_title text)"),
    "INSERT INTO mimiciv_hosp.d_icd_diagnoses VALUES ('X', 10, 'Example')",
    paste("INSERT INTO mimiciv_hosp.diagnoses_icd VALUES",
          "(1, 11, 1, 'X', 10), (2, 22, 1, 'X', 10), (2, 22, 6, 'X', 10)"),
    paste("INSERT INTO mimiciv_icu.icustays VALUES",
          "(1, 11, 101, '2020-01-02 00:00', '2020-01-02 08:00'),",
          "(1, 11, 102, '2020-01-02 10:00', '2020-01-02 18:00'),",
          "(2, 22, 201, '2020-01-02 00:00', '2020-01-02 08:00'),",
          "(3, 33, 301, '2020-01-02 00:00', '2020-01-02 08:00')"),
    paste("INSERT INTO mimiciv_derived.icustay_hourly VALUES",
          "(201, 0, '2020-01-02 01:00'), (201, 1, '2020-01-02 02:00')"),
    paste("INSERT INTO mimiciv_icu.chartevents VALUES",
          "(2, 22, 201, 1, '2020-01-02 00:00', 1),",
          "(2, 22, 201, 1, '2020-01-02 01:00', 10),",
          "(2, 22, 201, 1, '2020-01-02 02:00', 100),",
          "(2, 22, 201, 2, '2020-01-02 00:30', NULL),",
          "(2, 22, 201, 3, NULL, 5),",
          "(1, 11, 101, 20, '2020-01-02 00:30', 1),",
          "(2, 22, 201, 20, '2020-01-02 00:30', 1),",
          "(2, 22, 201, 21, '2020-01-02 00:30', 1),",
          "(3, 33, 301, 21, '2020-01-02 00:30', 1),",
          "(1, 11, 101, 22, '2020-01-02 00:30', 1),",
          "(3, 33, 301, 22, '2020-01-02 00:30', 1)"),
    paste("INSERT INTO mimiciv_hosp.labevents VALUES",
          "(1, 11, 10, '2020-01-02 09:00', 1),",
          "(2, 22, 11, NULL, 1),",
          "(2, 22, 12, '2020-01-02 00:00', 1),",
          "(2, 22, 12, '2020-01-02 01:00', 10),",
          "(2, 22, 12, '2020-01-02 02:00', 100),",
          "(2, 22, 13, '2020-01-02 00:30', NULL)")
  )
  for (statement in statements) dbExecute(con, statement)

  run_query <- function(table, itemid, op = "Exist", value = "", aggr = "",
                        hours = NULL) {
    schema <- get_table_schema(table)
    time_constraint <- list(
      is_time_constrained = !is.null(hours),
      time_min = if (is.null(hours)) NULL else hours[1],
      time_max = if (is.null(hours)) NULL else hours[2]
    )
    query <- query_generator(schema, table, itemid, op, "valuenum", value,
                             aggr, time_constraint, "numeric", NULL, con)
    dbGetQuery(con, query$sql, params = query$params)
  }
  stays <- function(x) sort(unique(x$stay_id))
  assert_stays <- function(actual, expected) {
    actual <- stays(actual)
    if (!identical(as.integer(actual), as.integer(expected))) {
      stop("Expected stays [", paste(expected, collapse = ", "),
           "], got [", paste(actual, collapse = ", "), "].")
    }
  }
  failures <- character()
  test <- function(name, code) {
    tryCatch({
      force(code)
      cat("PASS:", name, "\n")
    }, error = function(e) {
      failures <<- c(failures, name)
      cat("FAIL:", name, "-", conditionMessage(e), "\n")
    })
  }

  test("outcomes spinner preserves UI without hashing it", {
    ui <- shiny::tagList(shiny::div("Outcomes content"))
    shinycssloaders::withSpinner(ui, id = "warmup")
    trace("digest", where = asNamespace("digest"),
          tracer = quote(stop("Spinner unexpectedly hashed its UI.")), print = FALSE)
    tryCatch({
      first_id <- shiny::NS("first")("outcomes_spinner")
      second_id <- shiny::NS("second")("outcomes_spinner")
      first <- as.character(cohort_outcomes_spinner(ui, shiny::NS("first")))
      second <- as.character(cohort_outcomes_spinner(ui, shiny::NS("second")))
      stopifnot(first_id != second_id,
                grepl(paste0('data-spinner-id="', first_id, '"'), first, fixed = TRUE),
                grepl(paste0('data-spinner-id="', second_id, '"'), second, fixed = TRUE),
                grepl("Outcomes content", first, fixed = TRUE),
                grepl("Outcomes content", second, fixed = TRUE))
    }, finally = untrace("digest", where = asNamespace("digest")))
  })

  test("outcomes do not rerun when the stratification button initializes", {
    server <- function(input, output, session) {
      refresh <- cohort_outcomes_refresh(input)
      selected <- reactive(input$cohort)
      runs <- reactiveVal(0L)
      output$result <- renderText({
        runs(isolate(runs()) + 1L)
        paste(input$cohort, isolate(runs()))
      }) %>% bind_cohort_outcomes_events(selected, refresh)
    }
    testServer(server, {
      session$setInputs(cohort = "1")
      stopifnot(identical(output$result, "1 1"))
      session$setInputs(execute_stratification = 0)
      stopifnot(identical(output$result, "1 1"))
      session$setInputs(execute_stratification = 1)
      stopifnot(identical(output$result, "1 2"))
      session$setInputs(execute_stratification = 2)
      stopifnot(identical(output$result, "1 3"))
      session$setInputs(cohort = "2")
      stopifnot(identical(output$result, "2 4"))
      session$setInputs(execute_stratification = 0)
      stopifnot(identical(output$result, "2 4"))
      session$setInputs(execute_stratification = 1)
      stopifnot(identical(output$result, "2 5"))
    })
  })

  test("outcomes module binds cohort changes and explicit stratification requests", {
    calls <- 0L
    server <- cohortOutcomesExplorerServer
    overrides <- new.env(parent = environment(server))
    overrides$eventSearchbarServer <- function(...) reactive(NULL)
    overrides$cohort_outcome_query <- function(...) {
      calls <<- calls + 1L
      stop("query sentinel")
    }
    environment(server) <- overrides
    selected <- reactiveVal(NULL)
    testServer(server, args = list(database = reactive(NULL), cohort_id = selected), {
      result <- function() tryCatch(output$outcomes_result, error = identity)
      selected("1")
      session$flushReact()
      stopifnot(inherits(result(), "error"), calls == 1L)
      session$setInputs(execute_stratification = 0)
      stopifnot(inherits(result(), "error"), calls == 1L)
      session$setInputs(execute_stratification = 1)
      stopifnot(inherits(result(), "error"), calls == 2L)
      selected("2")
      session$flushReact()
      stopifnot(inherits(result(), "error"), calls == 3L)
    })
  })

  test("invalid condition chains explain missing operands and parentheses", {
    filter <- list(type = "filter", itemid = "1")
    and <- list(type = "condition", element = "AND")
    or <- list(type = "condition", element = "OR")
    open <- list(type = "enclose", element = "open")
    close <- list(type = "enclose", element = "close")
    invalid <- list(
      list(chain = list(filter, and), expected = "after AND"),
      list(chain = list(filter, or), expected = "after OR"),
      list(chain = list(and, filter), expected = "before AND"),
      list(chain = list(filter, and, or, filter), expected = "before OR"),
      list(chain = list(open, close), expected = "Parentheses"),
      list(chain = list(open, filter, and, close), expected = "after AND"),
      list(chain = list(open, filter), expected = "opening parenthesis"),
      list(chain = list(filter, close), expected = "closing parenthesis")
    )
    for (case in invalid) {
      error <- validate_condition_chain(case$chain)
      if (is.null(error) || !grepl(case$expected, error, fixed = TRUE)) {
        stop("Incorrect condition validation message for ", case$expected)
      }
    }
    valid <- list(
      list(), list(filter), list(filter, and, filter),
      list(filter, filter), list(filter, open, filter, or, filter, close),
      list(filter, and, open, filter, or, filter, close)
    )
    for (chain in valid) {
      if (!is.null(validate_condition_chain(chain))) stop("A valid condition was rejected.")
    }
  })
  test("search bar rejects trailing AND before querying the database", {
    inaccessible_database <- reactive({ stop("Unexpected database query") })
    testServer(eventSearchbarServer, args = list(database = inaccessible_database), {
      session$setInputs(filter_tojson = jsonlite::toJSON(
        list(message = "OK", result = list(
          list(type = "filter", itemid = "1"),
          list(type = "condition", element = "AND")
        )), auto_unbox = TRUE
      ))
      result <- condition_object()
      stopifnot(identical(result$condition_error,
                          "Add an event or parenthesized condition after AND."))
    })
    test("invalid cohort condition displays a message and clears stale results", {
      eventSearchbarServer <- function(...) {
        reactive(list(condition_error = "Add an event or parenthesized condition after AND."))
      }
      message_box <- function(title, message, ...) shiny::div(title, message)
      selected_profile <- function() NULL
      testServer(cohortCreationServer, args = list(database = reactive(NULL)), {
        fetched_cohort(data.frame(subject_id = 1))
        associated_sql_data(list(old = TRUE))
        session$setInputs(`event_searchbar-filter_tojson` = "invalid")
        result <- cohort_details_ui()
        stopifnot(grepl("Invalid cohort condition", as.character(result), fixed = TRUE),
                  grepl("after AND", as.character(result), fixed = TRUE),
                  is.null(fetched_cohort()), is.null(associated_sql_data()))
      })
      test("ICD filters match prefixes and versions in PostgreSQL", {
        dbExecute(con, paste(
          "INSERT INTO mimiciv_hosp.diagnoses_icd VALUES",
          "(1, 11, 2, 'ABC1', 10), (1, 11, 3, 'DEF1', 10),",
          "(2, 22, 2, 'ABC2', 10), (3, 33, 1, 'ABC3', 9)"
        ))
        cohort <- tibble::tibble(subject_id = 1:3, hadm_id = c(11L, 22L, 33L),
                                 stay_id = c(101L, 201L, 301L))
        condition <- function(allow = NULL, deny = NULL, allow_mode = "OR", deny_mode = "OR") {
          list(icd_to_allow = allow, icd_to_deny = deny,
               allow_condition = allow_mode, deny_condition = deny_mode)
        }
        assert_stays(process_icd_filter(function() con, condition(deny = "10 ABC"), cohort), 301)
        assert_stays(process_icd_filter(function() con, condition(allow = "10 ABC"), cohort), c(101, 201))
        assert_stays(process_icd_filter(function() con, condition(deny = "10 ABC,10 DEF",
                                                                 deny_mode = "AND"), cohort), c(201, 301))
        assert_stays(process_icd_filter(function() con, condition(allow = "10 ABC,10 DEF",
                                                                  allow_mode = "AND"), cohort), 101)
        assert_stays(process_icd_filter(function() con, condition(deny = "9 ABC"), cohort), c(101, 201))
        assert_stays(process_icd_filter(function() con, condition(deny = "10 ABC'"), cohort), c(101, 201, 301))
        assert_stays(process_icd_filter(function() con, condition(), cohort), c(101, 201, 301))
        dbExecute(con, "INSERT INTO mimiciv_hosp.diagnoses_icd VALUES (2, 22, 3, 'ABC9', 10)")
        assert_stays(process_icd_filter(function() con, condition(deny = "10 ABC,10 DEF",
                                                                 deny_mode = "AND"), cohort), c(201, 301))
      })
    })
  })

  test("column type lookups reuse the provided database connection", {
    before <- dbGetQuery(con, "SELECT count(*) AS n FROM pg_stat_activity WHERE datname = current_database()")$n
    for (i in 1:5) {
      if (!identical(get_column_type("valuenum", "mimiciv_icu", "chartevents", con), "numeric")) {
        stop("Unexpected column type.")
      }
    }
    after <- dbGetQuery(con, "SELECT count(*) AS n FROM pg_stat_activity WHERE datname = current_database()")$n
    if (after != before || !dbIsValid(con)) stop("Column type lookup changed the active connection count.")
  })
  test("cohort queries keep SQL metadata without printing it", {
    query <- get_constrained_table
    test_env <- new.env(parent = environment(query))
    test_env$check.numeric <- function(x) TRUE
    environment(query) <- test_env
    constraint <- list(
      itemid = 20, field = "valuenum", constraint = "Exist",
      value = "", aggr = "",
      time_constraint = list(is_time_constrained = FALSE)
    )
    output <- capture.output(result <- query(con, "chartevents", constraint))
    stopifnot(length(output) == 0L, nrow(result) > 0L,
              is.character(attr(result, "sql")), is.list(attr(result, "params")))
    constraint$field <- "not_allowed"
    error <- tryCatch(query(con, "chartevents", constraint), error = identity)
    stopifnot(inherits(error, "error"),
              grepl("not allowed", conditionMessage(error), fixed = TRUE))
  })
  test("cohort summary counts preserve distinct and missing ID behavior", {
    cohort <- dplyr::tbl(con, dbplyr::in_schema("public", "cohort"))
    for (id in c("1", "2", "3", "999")) {
      rows <- cohort %>% filter(cohort_id == !!id) %>% collect()
      expected <- vapply(rows[c("subject_id", "hadm_id", "stay_id")],
                         function(x) length(unique(x)), integer(1))
      actual <- get_cohort_explorer_counts(con, id)
      stopifnot(identical(as.numeric(actual[1, ]), as.numeric(expected)))
    }
  })
  test("whole-cohort outcomes and ICD counts preserve original join results", {
    d_cohorts <- dplyr::tbl(con, dbplyr::in_schema("public", "d_cohorts"))
    cohort <- dplyr::tbl(con, dbplyr::in_schema("public", "cohort"))
    demographics <- dplyr::tbl(con, dbplyr::in_schema("public", "demographics"))
    for (id in c("1", "2", "3", "999")) {
      original <- d_cohorts %>%
        filter(cohort_id == !!id) %>%
        inner_join(cohort, by = "cohort_id") %>%
        inner_join(demographics, by = c("subject_id", "hadm_id", "stay_id")) %>%
        mutate(strat = "Whole Cohort") %>%
        collect()
      optimized <- cohort_outcome_query(con, id)
      stopifnot(identical(colnames(optimized),
                          c("subject_id", "hadm_id", "stay_id", "label", "value", "valueuom")))
      columns <- c("subject_id", "hadm_id", "stay_id", "label", "value", "valueuom", "strat")
      expected <- original %>% select(all_of(columns)) %>% arrange(subject_id, hadm_id, stay_id, label)
      actual <- optimized %>% collect() %>% mutate(strat = "Whole Cohort") %>%
        select(all_of(columns)) %>% arrange(subject_id, hadm_id, stay_id, label)
      stopifnot(isTRUE(all.equal(as.data.frame(actual), as.data.frame(expected), check.attributes = FALSE)))
      local_icd <- get_cohort_icd_counts(con, original) %>% arrange(icd_code, strat)
      remote_icd <- get_cohort_icd_counts(con, cohort_icd_admissions_query(con, id)) %>%
        arrange(icd_code, strat)
      stopifnot(isTRUE(all.equal(as.data.frame(local_icd), as.data.frame(remote_icd), check.attributes = FALSE)))
    }
  })
  test("outcome rows pair characteristics by stay and retain strata", {
    data <- tibble::tibble(
      subject_id = c(1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L),
      hadm_id = c(11L, 22L, 11L, 11L, 22L, 11L, 11L, 11L, 11L),
      stay_id = c(101L, 201L, 102L, 101L, 201L, 101L, 102L, 101L, 101L),
      strat = c("A", "B", "B", "A", "B", "A", "B", "A", "A"),
      label = c("age", "gender", "age", "gender", "age", "age", "gender", "age", "age"),
      value = c("30", "M", NA, "F", "50", "31", "M", "30", NA),
      valueuom = c("y.o.", "", "y.o.", "", "y.o.", "y.o.", "", "y.o.", "y.o.")
    )
    expected <- tibble::tibble(
      strat = c("A", "B", "B"),
      `age (y.o.)` = c("30", NA, "50"),
      `gender ()` = c("F", "M", "M")
    )
    actual <- cohort_outcomes_wide(data) %>% select(all_of(names(expected)))
    reversed <- cohort_outcomes_wide(data[nrow(data):1L, ]) %>%
      select(all_of(names(expected)))
    stopifnot(identical(actual, expected), identical(reversed, expected))
    empty <- cohort_outcomes_wide(data[FALSE, ])
    stopifnot(nrow(empty) == 0L, identical(names(empty), "strat"))
    counts <- data %>% group_by(strat) %>%
      summarise(subjects = n_distinct(subject_id, na.rm = TRUE),
                admissions = n_distinct(hadm_id, na.rm = TRUE), .groups = "drop")
    stopifnot(identical(counts$subjects, c(1L, 2L)),
              identical(counts$admissions, c(1L, 2L)))
  })
  test("outcome rows match the previous reshape for aligned unique stays", {
    data <- tibble::tibble(
      subject_id = rep(c(1L, 2L), each = 2L),
      hadm_id = rep(c(11L, 22L), each = 2L),
      stay_id = rep(c(101L, 201L), each = 2L),
      strat = "Whole Cohort",
      label = rep(c("age", "gender"), 2L),
      value = c("30", "F", "50", "M"),
      valueuom = rep(c("y.o.", ""), 2L)
    )
    previous <- data %>% transmute(strat, label, value, valueuom) %>%
      group_by(strat, label, valueuom) %>% mutate(rn = row_number()) %>%
      pivot_wider(names_from = c("label", "valueuom"),
                  names_glue = "{label} ({valueuom})", values_from = "value") %>%
      select(-rn) %>% ungroup()
    actual <- cohort_outcomes_wide(data) %>% select(all_of(names(previous)))
    stopifnot(identical(actual, previous))
  })
  test("database outcomes retain duplicate, missing, and stratified data correctly", {
    dbExecute(con, "INSERT INTO public.d_cohorts VALUES (4, 'four')")
    dbExecute(con, paste(
      "INSERT INTO public.cohort VALUES",
      "(4, 1, 11, 101), (4, 1, 11, 101), (4, 2, 22, 201)"
    ))
    dbExecute(con, paste(
      "INSERT INTO public.demographics VALUES",
      "(1, 11, 101, 'age', '35', 'y.o.', 1),",
      "(1, 11, 101, 'age', NULL, 'y.o.', 1),",
      "(1, 11, 101, 'missing', NULL, '', 3),",
      "(2, 22, 201, 'gender', 'M', '', 2)"
    ))
    data <- cohort_outcome_query(con, "4") %>% collect() %>%
      mutate(strat = if_else(stay_id == 101L, "A", "B"))
    actual <- cohort_outcomes_wide(data) %>%
      select(strat, `age (y.o.)`, `gender ()`, `missing ()`)
    expected <- tibble::tibble(
      strat = c("A", "B"),
      `age (y.o.)` = c("35", "70"),
      `gender ()` = c("F", "M"),
      `missing ()` = c(NA_character_, NA_character_)
    )
    stopifnot(identical(actual, expected))
    icd <- get_cohort_icd_counts(con, data) %>% arrange(strat)
    stopifnot(identical(icd$strat, c("A", "B")), length(icd$n) == 2L,
              all(icd$n == 1L))
  })
  test("outcome ICD queries release temporary tables on repeated refresh", {
    population <- tibble::tibble(
      subject_id = c(1L, 1L, 2L), hadm_id = c(11L, 11L, 22L),
      strat = c("A", "A", "B")
    )
    temp_count <- function() dbGetQuery(con, paste(
      "SELECT count(*) AS n FROM pg_class",
      "WHERE relnamespace = pg_my_temp_schema() AND relkind = 'r'"
    ))$n
    before <- temp_count()
    for (i in 1:5) {
      result <- get_cohort_icd_counts(con, population)
      if (!identical(sort(result$strat), c("A", "B")) || any(result$n != 1L)) {
        stop("Incorrect ICD counts across strata.")
      }
      if (temp_count() != before) stop("Temporary tables were not removed.")
    }
    empty <- get_cohort_icd_counts(con, population[FALSE, ])
    if (nrow(empty) != 0L || temp_count() != before) {
      stop("Empty cohort returned ICD counts or left a temporary table.")
    }
  })
  test("patient explorer switches and refreshes cohorts without recreating the module", {
    runjs <- function(...) NULL
    custom_register_search <- function(session, data, search_query, endpoint_name, database = NULL) data
    selected <- reactiveVal(NULL)
    refresh <- reactiveVal(0L)
    selected_cohort <- reactive({ refresh(); selected() })
    testServer(patientExplorerServer, args = list(database = reactive(con), cohort_id = selected_cohort), {
      stopifnot(is.null(uid_list()), is.null(subject_id_list()))
      selected(1L)
      session$flushReact()
      stopifnot(setequal(uid_list()$uid, c(1L, 11L, 101L)),
                identical(subject_id_list()$subject_id, 1L),
                setequal(search_api_url_uid()$uid, c(1L, 11L, 101L)),
                identical(search_api_url_subject_id()$subject_id, 1L))
      timeline_itemid_list$data <- 99
      card_list$data <- list(99)
      selected(2L)
      session$flushReact()
      stopifnot(setequal(uid_list()$uid, c(2L, 22L, 201L)),
                identical(subject_id_list()$subject_id, 2L),
                setequal(search_api_url_uid()$uid, c(2L, 22L, 201L)),
                identical(search_api_url_subject_id()$subject_id, 2L),
                length(timeline_itemid_list$data) == 0L,
                length(card_list$data) == 0L)
      dbExecute(con, "INSERT INTO public.cohort VALUES (2, 3, 33, 301)")
      refresh(1L)
      session$flushReact()
      stopifnot(setequal(subject_id_list()$subject_id, c(2L, 3L)))
      selected(NULL)
      session$flushReact()
      stopifnot(is.null(uid_list()), is.null(subject_id_list()))
    })
  })

  test("overlapping attribution buffers preserve both stays", {
    assert_stays(run_query("labevents", 10), c(101, 102))
  })
  for (table in c("chartevents", "labevents")) {
    item <- if (table == "chartevents") 1 else 12
    test(paste(table, "hourly lower boundary included"), {
      assert_stays(run_query(table, item, "=", "1", hours = c(0, 0)), 201)
      assert_stays(run_query(table, item, "=", "10", hours = c(1, 1)), 201)
    })
    test(paste(table, "hourly upper boundary excluded"), {
      assert_stays(run_query(table, item, "=", "10", hours = c(0, 0)), integer())
      assert_stays(run_query(table, item, "=", "100", hours = c(1, 1)), integer())
    })
    test(paste(table, "hourly SUM counts boundary event once"), {
      assert_stays(run_query(table, item, "=", "11", "sum", c(0, 1)), 201)
      assert_stays(run_query(table, item, ">", "11", "sum", c(0, 1)), integer())
    })
    missing_value_item <- if (table == "chartevents") 2 else 13
    test(paste(table, "NULL numeric value does not satisfy comparison"), {
      assert_stays(run_query(table, missing_value_item, ">", "0", hours = c(0, 0)), integer())
      assert_stays(run_query(table, missing_value_item, ">", "0"), integer())
    })
    missing_time_item <- if (table == "chartevents") 3 else 11
    test(paste(table, "NULL event time cannot match an hourly window"), {
      assert_stays(run_query(table, missing_time_item, hours = c(0, 1)), integer())
    })
  }

  data <- setNames(lapply(20:22, function(item) run_query("chartevents", item)),
                   paste0("condition_", 1:3))
  universe <- dbGetQuery(con, "SELECT subject_id, hadm_id, stay_id FROM mimiciv_icu.icustays")
  condition_object <- function(expression, exclude = integer()) {
    list(condition_string = expression,
         constraint_list = setNames(lapply(1:3, function(i) {
           list(constraint = list(is_exclusion = i %in% exclude))
         }), as.character(1:3)))
  }
  test("AND selects the intersection", {
    assert_stays(apply_cohort_conditions(data, condition_object("condition_1 & condition_2")), 201)
  })
  test("OR selects the union", {
    assert_stays(apply_cohort_conditions(data, condition_object("condition_1 | condition_2")),
                 c(101, 201, 301))
  })
  test("OR returns admissions when one input lacks stay identifiers", {
    admissions <- tibble::tibble(subject_id = 1L, hadm_id = 11L)
    icu_stays <- tibble::tibble(subject_id = 2L, hadm_id = 22L, stay_id = 201L)
    for (result in list(orcondition(admissions, icu_stays), orcondition(icu_stays, admissions))) {
      stopifnot(setequal(result$subject_id, c(1L, 2L)),
                identical(result$stay_id[result$subject_id == 2L], 201L))
    }
  })
  test("EXCLUDE removes matching stays from the universe", {
    assert_stays(apply_cohort_conditions(
      data, condition_object("condition_1 & condition_2", exclude = 2), universe), 101)
  })
  test("EXCLUDE of an empty set retains the universe", {
    empty_data <- data
    empty_data$condition_2 <- data$condition_2[FALSE, ]
    assert_stays(apply_cohort_conditions(
      empty_data, condition_object("condition_2", exclude = 2), universe),
      c(101, 102, 201, 301))
  })
  test("EXCLUDE without a universe raises an error", {
    error <- tryCatch({
      apply_cohort_conditions(data, condition_object("condition_2", exclude = 2))
      NULL
    }, error = identity)
    if (is.null(error) || !grepl("universe is required", conditionMessage(error))) {
      stop("Expected an explicit error about the missing stay universe.")
    }
  })
  test("parentheses select A AND (B OR C)", {
    output <- capture.output(result <- apply_cohort_conditions(
      data, condition_object("condition_1 & ( condition_2 | condition_3 )")))
    stopifnot(length(output) == 0L)
    assert_stays(result, c(101, 201))
  })
  if (length(failures)) stop(length(failures), " cohort test(s) failed.")
  cat("All cohort creation tests passed.\n")
}, finally = {
  dbRollback(con)
  dbDisconnect(con)
})
