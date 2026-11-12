#!/usr/bin/env Rscript

library(DBI)
library(dplyr)

source("R/helpers/database.R")
source("R/helpers/logical.R")

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

  statements <- c(
    "CREATE SCHEMA mimiciv_icu",
    "CREATE SCHEMA mimiciv_hosp",
    "CREATE SCHEMA mimiciv_derived",
    paste("CREATE TABLE mimiciv_icu.icustays (subject_id integer, hadm_id integer,",
          "stay_id integer, intime timestamp, outtime timestamp)"),
    paste("CREATE TABLE mimiciv_icu.chartevents (subject_id integer, hadm_id integer,",
          "stay_id integer, itemid integer, charttime timestamp, valuenum numeric)"),
    paste("CREATE TABLE mimiciv_hosp.labevents (subject_id integer, hadm_id integer,",
          "itemid integer, charttime timestamp, valuenum numeric)"),
    paste("CREATE TABLE mimiciv_derived.icustay_hourly",
          "(stay_id integer, hr integer, endtime timestamp)"),
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
