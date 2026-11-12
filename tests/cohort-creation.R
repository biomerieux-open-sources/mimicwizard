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
  if (dbExistsTable(con, Id(schema = "public", table = "cohort"))) {
    stop("public.cohort already exists; refusing to alter this database.")
  }

  statements <- c(
    "CREATE SCHEMA mimiciv_icu",
    "CREATE SCHEMA mimiciv_hosp",
    "CREATE SCHEMA mimiciv_derived",
    paste("CREATE TABLE public.cohort (cohort_id integer, subject_id integer,",
          "hadm_id integer, stay_id integer)"),
    "INSERT INTO public.cohort VALUES (1, 1, 11, 101), (2, 2, 22, 201)",
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
    assert_stays(apply_cohort_conditions(
      data, condition_object("condition_1 & ( condition_2 | condition_3 )")), c(101, 201))
  })
  if (length(failures)) stop(length(failures), " cohort test(s) failed.")
  cat("All cohort creation tests passed.\n")
}, finally = {
  dbRollback(con)
  dbDisconnect(con)
})
