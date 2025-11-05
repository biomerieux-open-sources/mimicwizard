col_to_date <- function(col) {
  date <- parse_date_time(substring(col, 25),
                          orders = c("ymd"),
                          quiet = TRUE)
  if (all(is.na(date))) {
    return(col)
  } else {
    return(date)
  }
}

import_demo_data <- function(database, waiter) {
  demo_data_path <- "demo/"
  hosp_csv <- list.files(
    path = paste0(demo_data_path, "hosp"),
    pattern = "*.csv",
    full.names = TRUE
  )
  hosp_table_list <- setNames(lapply(hosp_csv, read_csv, col_types = cols(.default = col_character())), hosp_csv)

  icu_csv <- list.files(
    path = paste0(demo_data_path, "icu"),
    pattern = "*.csv",
    full.names = TRUE
  )
  icu_table_list <- setNames(lapply(icu_csv, readr::read_csv, show_col_types = FALSE),
                             icu_csv)

  hosp_table_list <- hosp_table_list  %>%
    map( ~ .x %>% mutate(across(where(is.character), col_to_date)))

  icu_table_list <- icu_table_list  %>%
    map( ~ .x %>% mutate(across(where(is.character), col_to_date)))


  current <- 0
  total <- length(hosp_csv) + length(icu_csv)

  for (csv_path in names(hosp_table_list)) {
    table_name <- sub(".*/(.*?)\\.csv\\.gz$", "\\1", csv_path)
    dbAppendTable(database(init_func=TRUE),
                  Id(schema = "mimiciv_hosp", table = table_name),
                  hosp_table_list[[csv_path]])
    current <- current + 1
    waiter$update(html = tagList(
      spin_pixel(),
      paste0(
        "Importing data from 'demo' folder ",
        current,
        "/",
        total,
        " (demo-mode)"
      )
    ))
  }

  for (csv_path in names(icu_table_list)) {
    table_name <- sub(".*/(.*?)\\.csv\\.gz$", "\\1", csv_path)
    dbAppendTable(database(init_func=TRUE),
                  Id(schema = "mimiciv_icu", table = table_name),
                  icu_table_list[[csv_path]])
    current <- current + 1
    waiter$update(html = tagList(
      spin_pixel(),
      paste0(
        "Importing data from 'demo' folder ",
        current,
        "/",
        total,
        " (demo-mode)"
      )
    ))
  }

}

getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ){
      break
    }

    line <- gsub("\\t", " ", line)

    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }

    sql.string <- paste(sql.string, line)
  }

  close(con)
  return(sql.string)
}

create_demo_configuration <- function(database) {
  sql_request <- readLines("R/helpers/demo_app_table_creation.sql")
  #Remove comment
  clean_sql_request <- gsub("(--[^\\n]*|/\\*.*?\\*/)", "", sql_request, perl = TRUE)
  clean_sql_request <- paste(clean_sql_request, collapse = " ")[1]
  requests <- strsplit(clean_sql_request, ";", fixed = TRUE)[[1]]
  for (request in requests) {
    if (nchar(trimws(request)) > 0) {
      dbExecute(database(init_func=TRUE), request)
    }
  }
}

define_postgres_function <- function(database) {
  sql_request <- readLines("R/helpers/demo_postgres_func.sql")
  clean_sql_request <- gsub("(--[^\\n]*|/\\*.*?\\*/)", "", sql_request, perl = TRUE)
  clean_sql_request <- paste(clean_sql_request, collapse = " ")[1]
  requests <- strsplit(clean_sql_request, "LANGUAGE PLPGSQL;", fixed = TRUE)[[1]]
  for (request in requests) {
    if (nchar(trimws(request)) > 0) {
      dbExecute(database(init_func=TRUE), paste(request, "LANGUAGE PLPGSQL;"),immediate=TRUE)
    }
  }
}

expand_sql_file <- function(path, base_dir = dirname(path)) {
  lines <- readLines(path)

  result <- character()
  for (line in lines) {
    line <- trimws(line)
    if (startsWith(line, "\\i ")) {
      include_path <- sub("^\\\\i\\s+", "", line)
      full_path <- file.path(base_dir, include_path)
      result <- c(result, expand_sql_file(full_path, dirname(full_path)), ";")
    } else if (!startsWith(line, "\\echo") && !startsWith(line, "--") && !startsWith(line, "/*")) {
      result <- c(result, line)
    }
  }
  return(result)
}

create_demo_db_views <- function(database) {
  sql_lines <- expand_sql_file("R/helpers/concepts_postgres/postgres-make-concepts.sql")
  sql_string <- paste(sql_lines, collapse = "\n")
  sql_string <- gsub(";", ";\n", sql_string)

  # Now split and trim
  statements <- unlist(strsplit(sql_string, ";", fixed = TRUE))
  statements <- trimws(statements)
  statements <- statements[nchar(statements) > 0]

    for (stmt in statements) {
      dbExecute(database(init_func=TRUE), stmt)
    }


  sql_request <- readLines("R/helpers/demo_materialized_view.sql")
  clean_sql_request <- gsub("(--[^\\n]*|/\\*.*?\\*/)", "", sql_request, perl = TRUE)
  clean_sql_request <- paste(clean_sql_request, collapse = " ")[1]
  requests <- strsplit(clean_sql_request, ";", fixed = TRUE)[[1]]
  for (request in requests) {
    if (nchar(trimws(request)) > 0) {
      dbExecute(database(init_func=TRUE), request,immediate=FALSE)
    }
  }
}

create_demo_data_structure <- function(database) {
  sql_request <- readLines("R/helpers/demo_mimic_table_creation.sql")
  clean_sql_request <- gsub("(--[^\\n]*|/\\*.*?\\*/)", "", sql_request, perl = TRUE)
  clean_sql_request <- paste(clean_sql_request, collapse = " ")[1]
  requests <- strsplit(clean_sql_request, ";", fixed = TRUE)[[1]]
  for (request in requests) {
    if (nchar(trimws(request)) > 0) {
      dbExecute(database(init_func=TRUE), request)
    }
  }
}



connect_to_mimic <- function(waiter = NULL) {

  if (CONFIG$DATABASE_MODE == "DEMO") {
    if(!is.null(waiter)){
      waiter$update(html = tagList(
        spin_pixel(),
        "Connecting to local database (demo-mode)"
      ))
    }
    drv <- RPostgres::Postgres()
    demo_database <- dbConnect(
      drv,
      dbname = CONFIG$DEMO_DBNAME,
      host = CONFIG$DEMO_HOST,
      port = CONFIG$DEMO_PORT,
      user = CONFIG$DEMO_USER,
      password = CONFIG$DEMO_PASSWORD
    )

    demo_database
  }
  else{
    if(!is.null(waiter)){
      waiter$update(html = tagList(
        spin_pixel(),
        "Connecting to hosted MIMIC-IV database"
      ))
    }
    print("Request new db connection")
    drv <- RPostgres::Postgres()
    database <- dbConnect(
      drv,
      dbname = CONFIG$HOSTED_DBNAME,
      host = CONFIG$HOSTED_HOST,
      port = CONFIG$HOSTED_PORT,
      user = CONFIG$HOSTED_USER,
      password = CONFIG$HOSTED_PASSWORD
    )
    if(!is.null(waiter)){
      waiter$update(html = tagList(
        spin_pixel(),
        "Connected ! Starting app..."
      ))
    }
    database
  }
}



get_table_schema <- function(table) {
  if (table %in% list("labevents","microbiologyevents","prescriptions","d_labitems")) {
    "mimiciv_hosp"
  } else if (table %in% list("customevents","d_customevents","microbiologyresultsevents","d_prescriptions","demographics")) {
    "public"
  } else if (table %in% list(
    "procedureevents",
    "ingredientevents",
    "inputevents",
    "outputevents",
    "datetimeevents",
    "chartevents",
    "d_items"
  )) {
    "mimiciv_icu"
  } else{
    stop("No registered schema for this table name")
  }
}

get_aggregate_having_clause <- function(field_sql, aggr, database) {
  if (aggr == "") return(field_sql)

  if (aggr == "median") {
    paste0("HAVING PERCENTILE_CONT(0.5) WITHIN GROUP(ORDER BY ",field_sql,")")
  } else {
    agg_fun <- switch(aggr,
                      min = "MIN",
                      mean = "AVG",
                      max = "MAX",
                      sum = "SUM",
                      stop("Invalid aggregation"))
    paste0("HAVING ",agg_fun,"(",field_sql,")")
  }
}


query_generator <-
  function(schema,
           linksto,
           itemid,
           constraint,
           field,
           value,
           aggr,
           time_constraint,
           column_type,
           cohort_filter,
           database) {

    param_counter <- 1
    params <- list()

    get_param <- function(val) {
      params[[param_counter]] <<- val
      placeholder <- paste0("$", param_counter)
      param_counter <<- param_counter + 1
      return(placeholder)
    }

    ## --- WHERE clause generation
    if (column_type %in% list("character varying", "text")) {
      if (constraint == "Contains") {
        where <- paste0(field, " LIKE '%' || ", get_param(value), " || '%'")
      } else if (constraint == "Not Contains") {
        where <- paste0(field, " NOT LIKE '%' || ", get_param(value), " || '%'")
      } else if (constraint == "Is") {
        where <- paste0("UPPER(TRIM(", field, ")) = UPPER(TRIM(", get_param(value), "))")
      } else if (constraint == "Is Not") {
        where <- paste0("UPPER(TRIM(", field, ")) <> UPPER(TRIM(", get_param(value), "))")
      } else if (constraint == "Exist") {
        where <- "1 = 1"
      } else if (constraint == "Is True") {
        where <- paste0(field, " = ", get_param(1))
      } else if (constraint == "Is False") {
        where <- paste0(field, " = ", get_param(0))
      } else {
        stop(paste(constraint," is an unknown constraint for text column"))
      }

      # Handle numeric interpretation
      field_numeric <- paste0(
        "CASE WHEN ", field,
        " ~ E'^[+-]?([0-9]*[.])?[0-9]+$' THEN ",
        field, "::numeric ELSE NULL END"
      )

      extended_field <- get_aggregate_having_clause(field_numeric, aggr, database)
      if (constraint %in% c("=", "<>", "<", ">", "<=", ">=")) {
        operator <- constraint
        where <- paste0(extended_field, " ", operator, " ", get_param(value))
      }

    } else {
      # Numeric columns
      extended_field <- get_aggregate_having_clause(field, aggr, database)
      if (constraint == "Exist") {
        where <- "1 = 1"
      } else if (constraint == "Is True") {
        where <- paste0(extended_field, " = ", get_param(1))
      } else if (constraint == "Is False") {
        where <- paste0(extended_field, " = ", get_param(0))
      } else if (constraint %in% c("=", "<>", "<", ">", "<=", ">=")) {
        operator <- constraint
        where <- paste0(extended_field, " ", operator, " ", get_param(value))
      } else {
        stop(paste(constraint," is an unknown constraint for numeric column (",extended_field,"). Please use a numeric constraint."))
      }
    }

    ## --- ICU Time filtering
    icu_time_join <- ""
    icu_time_where <- ""

    if (time_constraint$is_time_constrained == TRUE) {
      if (schema %in% c("mimiciv_hosp", "public")) {
        icu_time_join <- paste0(
          " JOIN (SELECT stay_id, hr, endtime - interval '1 hour' AS starttime, endtime FROM mimiciv_derived.icustay_hourly ih) ih
          ON (i.stay_id = ih.stay_id AND t.", event_time_target[[linksto]], " BETWEEN ih.starttime AND ih.endtime) "
        )
      } else {
        icu_time_join <- paste0(
          " JOIN (SELECT stay_id, hr, endtime - interval '1 hour' AS starttime, endtime FROM mimiciv_derived.icustay_hourly ih) ih
          ON (t.stay_id = ih.stay_id AND t.", event_time_target[[linksto]], " BETWEEN ih.starttime AND ih.endtime) "
        )
      }

      icu_time_where <- paste0(" AND ih.hr BETWEEN ", get_param(time_constraint$time_min), " AND ", get_param(time_constraint$time_max))
    }

    ## --- Cohort filter
    cohort_join <- ""
    cohort_where <- ""
    if (!is.null(cohort_filter)) {
      if (schema %in% c("mimiciv_hosp", "public")) {
        cohort_join <- " JOIN public.cohort c ON (c.subject_id = i.subject_id AND c.hadm_id = i.hadm_id AND c.stay_id = i.stay_id) "
      } else {
        cohort_join <- " JOIN public.cohort c ON (c.subject_id = t.subject_id AND c.hadm_id = t.hadm_id AND c.stay_id = t.stay_id) "
      }
      cohort_where <- paste0(" AND cohort_id = ", get_param(cohort_filter))
    }

    ## --- Main Query
    itemid_clause <- paste0("itemid = ", get_param(itemid))
    main_select <- if (schema %in% c("mimiciv_hosp", "public")) {
      paste0(
        "SELECT DISTINCT i.subject_id AS subject_id, i.hadm_id AS hadm_id, i.stay_id AS stay_id FROM ",
        schema, ".", linksto, " t ",
        "LEFT JOIN mimiciv_icu.icustays i ON (i.subject_id = t.subject_id AND t.hadm_id = i.hadm_id AND ",
        event_time_target[[linksto]], " BETWEEN intime - interval '24 hour' AND outtime + interval '1 hour') ",
        ifelse(linksto == "prescriptions", " JOIN public.d_prescriptions USING(drug) ", ""),
        icu_time_join,
        cohort_join,
        " WHERE ", itemid_clause,
        icu_time_where,
        cohort_where,
        if (startsWith(where, "HAVING")) {
          paste0(" GROUP BY i.subject_id, i.hadm_id, i.stay_id ", where)
        } else {
          paste0(" AND ", where)
        }
      )
    } else {
      paste0(
        "SELECT DISTINCT t.subject_id AS subject_id, t.hadm_id AS hadm_id, t.stay_id AS stay_id FROM ",
        schema, ".", linksto, " t ",
        icu_time_join,
        cohort_join,
        " WHERE ", itemid_clause,
        icu_time_where,
        cohort_where,
        if (startsWith(where, "HAVING")) {
          paste0(" GROUP BY t.subject_id, t.hadm_id, t.stay_id ", where)
        } else {
          paste0(" AND ", where)
        }
      )
    }

    ## --- Final Output
    return(list(sql = main_select, params = params))

    # ####"
    # if (column_type %in% list("character varying","text")) {
    #   # String column
    #   #field, value
    #   # String operator
    #   if (constraint == "Contains") {
    #     where <- paste0(field, " LIKE '%", value, "%'")
    #   }
    #   if (constraint == "Not Contains") {
    #     where <- paste0(field, " NOT LIKE '%", value, "%'")
    #   }
    #   if (constraint == "Is") {
    #     where <-
    #       paste0("UPPER(TRIM(", field, ")) = UPPER(TRIM('", value, "'))")
    #   }
    #   if (constraint == "Is Not") {
    #     where <-
    #       paste0("UPPER(TRIM(", field, ")) <> UPPER(TRIM('", value, "'))")
    #   }
    #   if (constraint == "Exist") {
    #     where <- paste0(" 1 = 1 ")
    #   }
    #   if (constraint == "Is True") {
    #     where <- paste0(field, " = '", 1, "'")
    #   }
    #   if (constraint == "Is False") {
    #     where <- paste0(field, " = '", 0, "'")
    #   }
    #
    #   # Numeric operator
    #   field <-
    #     paste0(
    #       "CASE WHEN ",
    #       field,
    #       "~E'^[+-]?([0-9]*[.])?[0-9]+$' THEN ",
    #       field,
    #       "::numeric ELSE NULL end"
    #     )
    #   extended_field <- get_aggregate_having_clause(field, aggr,database)
    #   if (constraint == "=") {
    #     where <- paste0(extended_field, " = ", value)
    #   }
    #   if (constraint == ">") {
    #     where <- paste0(extended_field, " > ", value)
    #   }
    #   if (constraint == "<") {
    #     where <- paste0(extended_field, " < ", value)
    #   }
    #   if (constraint == ">=") {
    #     where <- paste0(extended_field, " >= ", value)
    #   }
    #   if (constraint == "<=") {
    #     where <- paste0(extended_field, " <= ", value)
    #   }
    #   if (constraint == "<>") {
    #     where <- paste0(extended_field, " <> ", value)
    #   }
    # } else{
    #   # Numeric Column
    #   extended_field <- get_aggregate_having_clause(field, aggr)
    #   if (constraint == "Exist") {
    #     where <- paste0(" 1 = 1 ")
    #   }
    #   if (constraint == "Is True") {
    #     where <- paste0(extended_field, ' = ', 1)
    #   }
    #   if (constraint == "Is False") {
    #     where <- paste0(extended_field, ' = ', 0)
    #   }
    #   if (constraint == "=") {
    #     where <- paste0(extended_field, " = ", value)
    #   }
    #   if (constraint == ">") {
    #     where <- paste0(extended_field, " > ", value)
    #   }
    #   if (constraint == "<") {
    #     where <- paste0(extended_field, " < ", value)
    #   }
    #   if (constraint == ">=") {
    #     where <- paste0(extended_field, " >= ", value)
    #   }
    #   if (constraint == "<=") {
    #     where <- paste0(extended_field, " <= ", value)
    #   }
    #   if (constraint == "<>") {
    #     where <- paste0(extended_field, " <> ", value)
    #   }
    # }
    # icu_time_join <- ""
    # icu_time_where <- ""
    # if (time_constraint$is_time_constrained == T) {
    #   if (schema == "mimiciv_hosp" | schema == "public") {
    #     icu_time_join <-
    #       paste0(
    #         " JOIN (SELECT stay_id,hr,endtime - interval '1 hour' AS starttime,endtime FROM mimiciv_derived.icustay_hourly ih)  ih
    #                    ON (i.stay_id = ih.stay_id AND t.",
    #         event_time_target[[linksto]],
    #         " BETWEEN ih.starttime AND ih.endtime) "
    #       )
    #   } else{
    #     icu_time_join <-
    #       paste0(
    #         " JOIN (SELECT stay_id,hr,endtime - interval '1 hour' AS starttime,endtime FROM mimiciv_derived.icustay_hourly ih)  ih
    #                    ON (t.stay_id = ih.stay_id AND t.",
    #         event_time_target[[linksto]],
    #         " BETWEEN ih.starttime AND ih.endtime) "
    #       )
    #   }
    #   icu_time_where <-
    #     paste(" AND ih.hr BETWEEN",
    #           time_constraint$time_min,
    #           "AND",
    #           time_constraint$time_max)
    # }
    #
    #
    #
    # prefix <- ""
    # cohort_join <- ""
    # cohort_where <- ""
    #
    # if (schema == "mimiciv_hosp" | schema == "public") {
    #   if (!is.null(cohort_filter)) {
    #     cohort_join <-
    #       " JOIN public.cohort c ON(c.subject_id = i.subject_id and c.hadm_id = i.hadm_id and c.stay_id = i.stay_id)  "
    #     cohort_where <- paste0(" AND cohort_id = ", cohort_filter)
    #   }
    #   paste0(
    #     'SELECT DISTINCT i.subject_id as ',
    #     prefix,
    #     'subject_id,i.hadm_id as ',
    #     prefix,
    #     'hadm_id,i.stay_id as ',
    #     prefix,
    #     'stay_id FROM ',
    #     schema,
    #     '.',
    #     linksto,
    #     ' t ',
    #     "LEFT JOIN mimiciv_icu.icustays i ON ( i.subject_id = t.subject_id and t.hadm_id = i.hadm_id and ",event_time_target[[linksto]]," between intime - interval '24 hour' and outtime + interval '1 hour')",
    #     ifelse(linksto=="prescriptions"," JOIN public.d_prescriptions USING(drug) ",""),
    #     icu_time_join,
    #     cohort_join,
    #     ' WHERE itemid = ',
    #     itemid,
    #     icu_time_where,
    #     cohort_where,
    #     ifelse(
    #       startsWith(where, "HAVING"),
    #       paste0(" GROUP BY i.subject_id,i.hadm_id,i.stay_id ", where),
    #       paste0(' AND ', where)
    #     )
    #
    #   )
    # } else{
    #   if (!is.null(cohort_filter)) {
    #     cohort_join <-
    #       " JOIN public.cohort c ON(c.subject_id = t.subject_id and c.hadm_id = t.hadm_id and c.stay_id = t.stay_id)  "
    #     cohort_where <- paste0(" AND cohort_id = ", cohort_filter)
    #   }
    #   paste0(
    #     'SELECT DISTINCT t.subject_id as ',
    #     prefix,
    #     'subject_id,t.hadm_id as ',
    #     prefix,
    #     'hadm_id,t.stay_id as ',
    #     prefix,
    #     'stay_id FROM ',
    #     schema,
    #     '.',
    #     linksto,
    #     ' t ',
    #     icu_time_join,
    #     cohort_join,
    #     ' WHERE itemid = ',
    #     itemid,
    #     icu_time_where,
    #     cohort_where,
    #     ifelse(
    #       startsWith(where, "HAVING"),
    #       paste0(" GROUP BY t.subject_id,t.hadm_id,t.stay_id ", where),
    #       paste0(' AND ', where)
    #     )
    #   )
    # }

  }

get_column_type <- function(field, schema, table) {
  database <- connect_to_mimic()
  column_type_query <-
    paste0('SELECT pg_typeof("',
           field,
           '")
                        from ',
           schema,
           '.',
           table,
           ' i
                        LIMIT 1')
  #String = character varying
  as.character(dplyr::tbl(database, sql(column_type_query)) %>% collect())
}

allowed_target <- list(
  "inputevents" = list("amount", "rate"),
  "chartevents" = list("value", "valuenum"),
  "labevents" = list("value", "valuenum"),
  "ingredientevents" = list("amount", "rate"),
  "outputevents" = list("value"),
  "datetimeevents" = list("value"),
  "procedureevents" = list("value", "location"),
  "microbiologyresultsevents" = list("value", "valuenum"),
  "prescriptions" = list("dose_val_rx","doses_per_24_hrs"),
  "customevents" = list("value"),
  "demographics" = list("value")
)

get_constrained_table <-
  function(database,
           linksto,
           constraint,
           cohort_filter = NULL) {
    schema <- get_table_schema(linksto)
    if (check.numeric(constraint$itemid)) {
      if(constraint$field %in% unique(unlist(allowed_target))){
        column_type <-
          get_column_type(constraint$field, schema, linksto)
        query <-
          query_generator(
            schema,
            linksto,
            constraint$itemid,
            constraint$constraint,
            constraint$field,
            constraint$value,
            constraint$aggr,
            constraint$time_constraint,
            column_type,
            cohort_filter,
            database
          )
        print(query)
        dbGetQuery(database, query$sql, params = query$params)
      } else{
        stop(paste("request on this field is not allowed :",constraint$field))
      }

    } else{
      stop(paste("item id should be numeric, current value :",constraint$itemid))
    }

  }

request_constrained <-
  function(condition_object,
           progress = NULL,
           is_synchronous = F,
           cohort_filter = NULL) {
    condition_string <- condition_object$condition_string
    table_list <- list()
    if(condition_string != ""){
    constraint_list <-
      condition_object$constraint_list
    table_count <- length(constraint_list)
    if (!is.null(progress)) {
      current_progress <- progress
    }
      for (condition_id in 1:table_count) {
        if (is_synchronous) {

          dedicated_db_link <- connect_to_mimic()
          data <-
            get_constrained_table(
              dedicated_db_link,
              constraint_list[[as.character(condition_id)]]$linksto,
              constraint_list[[as.character(condition_id)]]$constraint,
              cohort_filter
            ) %>% collect()

          DBI::dbDisconnect(dedicated_db_link)
          table_list[[paste0("condition_", condition_id)]] <- data
        } else{
          table_list[[paste0("condition_", condition_id)]] <-
            promises::future_promise(
              globals = c(
                "condition_id",
                "constraint_list",
                "cohort_filter",
                "table_count",
                "connect_to_mimic",
                "get_constrained_table",
                "get_table_schema",
                "get_column_type",
                "query_generator",
                "progress",
                "get_aggregate_having_clause",
                "event_time_target",
                "demo_database",
                "DATABASE_MODE",
                "APPLICATION_MODE"
              ),
              packages = c("dplyr", "dbplyr", "DBI"),
              {
                start.time <- Sys.time()
                dedicated_db_link <- connect_to_mimic()
                data <-
                  get_constrained_table(
                    dedicated_db_link,
                    constraint_list[[as.character(condition_id)]]$linksto,
                    constraint_list[[as.character(condition_id)]]$constraint,
                    cohort_filter
                  ) %>% collect()

                DBI::dbDisconnect(dedicated_db_link)
                end.time <- Sys.time()
                time.taken <- end.time - start.time
                print(paste("request time :", time.taken))
                data
              }
            ) %>% then(if (!is.null(progress)) {
              current_progress$inc(
                amount = 3 / table_count,
                message = paste0(
                  "Requesting database - Filter ",
                  round((current_progress$getValue() - 1) / (3 / table_count)),
                  "/",
                  table_count
                )
              )
            })
        }
      }
    }
    table_list
  }

event_time_target <- list(
  "inputevents" = "starttime",
  "chartevents" = "charttime",
  "labevents" = "charttime",
  "ingredientevents" = "starttime",
  "outputevents" = "charttime",
  "datetimeevents" = "charttime",
  "procedureevents" = "starttime",
  "microbiologyevents" = "charttime",
  "microbiologyresultsevents" = "charttime",
  "customevents" = "charttime",
  "prescriptions" = "starttime",
  "demographics" = "charttime"
)

event_descriptor_target <- list(
  "inputevents" = "d_items",
  "chartevents" = "d_items",
  "labevents" = "d_labitems",
  "ingredientevents" = "d_items",
  "outputevents" = "d_items",
  "datetimeevents" = "d_items",
  "procedureevents" = "d_items",
  "customevents" = "d_customevents",
  "microbiologyevents" = NULL,
  "microbiologyresultsevents" = NULL,
  "demographics" = NULL,
  "prescriptions" = "d_prescriptions"
)
event_value_target <- list(
  "inputevents" = "amount",
  "chartevents" = "valuenum",
  "labevents" = "valuenum",
  "ingredientevents" = "amount",
  "outputevents" = "value",
  "datetimeevents" = "value",
  "procedureevents" = "value",
  "customevents" = "value",
  "demographics" = "value",
  "microbiologyevents" = "value",
  "prescriptions" = "dose_val_rx"
)
target_to_uom <- list(
  "amount" = "amountuom",
  "rate" = "rateuom",
  "value" = "valueuom",
  "valuenum" = "valueuom",
  "dose_val_rx" = "dose_unit_rx"
)
request_event_from_data <-
  function(database,
           subject_id,
           hadm_id,
           stay_id,
           itemid,
           linksto,
           time_value,
           endtime,
           value,
           valueuom) {
    if (is.na(linksto) || is.null(linksto))
      linksto <- "labevents"
    time_target <- event_time_target[[linksto]]
    value_target <- event_value_target[[linksto]]
    uom_target <- target_to_uom[[value_target]]
    itemid <- as.integer(itemid)
    schema <- get_table_schema(linksto)
    if(!is.null(event_descriptor_target[[linksto]]))
      d_schema <- get_table_schema(event_descriptor_target[[linksto]])

     if (linksto == "microbiologyevents") {
      dplyr::tbl(database, in_schema(schema, linksto)) %>%
        filter(microevent_id == !!valueuom) %>%
        collect()
    } else if(linksto == "customevents"){
      dplyr::tbl(database, in_schema(schema, linksto)) %>%
        filter(
          subject_id == !!subject_id &&
            hadm_id == !!hadm_id &&
            itemid == !!itemid &&
            .data[[time_target]] == !!time_value &&
            .data[[value_target]] == !!value

        ) %>%
        inner_join(dplyr::tbl(database, in_schema(schema, event_descriptor_target[[linksto]])), by =
                     "itemid") %>%
        collect()
    } else if(linksto == "demographics"){
      dplyr::tbl(database, in_schema(schema, linksto)) %>%
        filter(
          subject_id == !!subject_id &&
            hadm_id == !!hadm_id &&
            itemid == !!itemid &&
            .data[[time_target]] == !!time_value &&
            .data[[value_target]] == !!value
        ) %>%
        collect()
    }else if (linksto == "datetimeevents") {
      dplyr::tbl(database, in_schema(schema, linksto)) %>%
        filter(
          subject_id == !!subject_id &&
            hadm_id == !!hadm_id &&
            itemid == !!itemid &&
            .data[[time_target]] == !!time_value &&
            stay_id == !!stay_id &&
            {
              ifelse(is.null(!!rlang::parse_expr(uom_target)),
                     is.null(.data[[uom_target]]),
                     .data[[uom_target]] == !!valueuom)
            }
        ) %>%
        inner_join(dplyr::tbl(database, in_schema(schema, event_descriptor_target[[linksto]])), by =
                     "itemid") %>%
        collect()
    } else if (time_target == "charttime") {
      dplyr::tbl(database, in_schema(schema, linksto)) %>%
        filter(
          subject_id == !!subject_id &&
            #hadm_id == !!hadm_id && Removed for retrieve lab results from ED
            itemid == !!itemid &&
            .data[[time_target]] == !!time_value &&
            {
              ifelse(
                is.null(!!rlang::parse_expr(value_target)),
                is.null(.data[[value_target]]),
                as.double(.data[[value_target]]) == as.double(!!value)
              )
            } &&
            {
              ifelse(is.null(!!rlang::parse_expr(uom_target)),
                     is.null(.data[[uom_target]]),
                     .data[[uom_target]] == !!valueuom)
            }
        ) %>%
        {
          if (schema == "mimiciv_icu")
            filter(., stay_id == !!stay_id)
          else
            .
        } %>%
        inner_join(dplyr::tbl(database, in_schema(schema, event_descriptor_target[[linksto]])), by =
                     "itemid") %>%
        collect()
    } else{

      dplyr::tbl(database, in_schema(schema, linksto)) %>%
        filter(
          subject_id == !!subject_id &&
            hadm_id == !!hadm_id &&
            itemid == !!itemid &&
            .data[[time_target]] == !!time_value &&
            {
              ifelse(
                is.null(!!rlang::parse_expr(value_target)),
                is.null(.data[[value_target]]),
                .data[[value_target]] == !!value
              )
            } &&
            {
              ifelse(is.null(!!rlang::parse_expr(uom_target)),
                     is.null(.data[[uom_target]]),
                     .data[[uom_target]] == !!valueuom)
            }
        ) %>%
        {
          if (schema == "mimiciv_icu")
            filter(., stay_id == stay_id)
          else
            .
        } %>%
        {
          if (linksto == "prescriptions")
            filter(., stoptime == !!endtime)
          else
            filter(., endtime == !!endtime)
        } %>%
        inner_join(dplyr::tbl(database, in_schema(d_schema, event_descriptor_target[[linksto]])), by =
                     ifelse(linksto == "prescriptions","drug","itemid")) %>%
        collect()
    }
  }


# Used for patient event server to display available events
request_compressed_events <-
  function(database,
           subject_id,
           hadm_id = NULL,
           stay_id = NULL) {
    patients <-
      dplyr::tbl(database, in_schema("mimiciv_hosp", "patients"))
    admissions <-
      dplyr::tbl(database, in_schema("mimiciv_hosp", "admissions"))
    d_items <-
      dplyr::tbl(database, in_schema("mimiciv_icu", "d_items"))
    d_labitems <-
      dplyr::tbl(database, in_schema("mimiciv_hosp", "d_labitems"))
    d_prescriptions <-
      dplyr::tbl(database, in_schema("public", "d_prescriptions"))
    d_customevents <-
      dplyr::tbl(database, in_schema("public", "d_customevents"))

    if (!is.null(hadm_id)) {
      procedureevents <-
        dplyr::tbl(database, in_schema("mimiciv_icu", "procedureevents")) %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        {
          if (!is.null(stay_id))
            filter(., stay_id == !!stay_id)
          else
            .
        } %>%
        inner_join(d_items, by = "itemid") %>%
        transmute(
            subject_id,
            hadm_id,
            stay_id,
            itemid,
            label,
            abbreviation,
            linksto,
            category,
            unitname,
            param_type,
            lownormalvalue,
            highnormalvalue,
            value = (value),
            valueuom,
            starttime = as.POSIXct(starttime),
            endtime = as.POSIXct(endtime)
        )

      ingredientevents <-
        dplyr::tbl(database, in_schema("mimiciv_icu", "ingredientevents")) %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        {
          if (!is.null(stay_id))
            filter(., stay_id == !!stay_id)
          else
            .
        } %>%
        inner_join(d_items, by = "itemid") %>%
        transmute(
            subject_id,
            hadm_id,
            stay_id,
            itemid,
            label,
            abbreviation,
            linksto,
            category,
            unitname,
            param_type,
            lownormalvalue,
            highnormalvalue,
            value = (amount),
            valueuom = amountuom,
            starttime = as.POSIXct(starttime),
            endtime = as.POSIXct(endtime)
        )

      inputevents <-
        dplyr::tbl(database, in_schema("mimiciv_icu", "inputevents")) %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        {
          if (!is.null(stay_id))
            filter(., stay_id == !!stay_id)
          else
            .
        } %>%
        inner_join(d_items, by = "itemid") %>%
        transmute(
            subject_id,
            hadm_id,
            stay_id,
            itemid,
            label,
            abbreviation,
            linksto,
            category,
            unitname,
            param_type,
            lownormalvalue,
            highnormalvalue,
            value = (amount),
            valueuom = amountuom,
            starttime = as.POSIXct(starttime),
            endtime = as.POSIXct(endtime)
        )

      outputevents <-
        dplyr::tbl(database, in_schema("mimiciv_icu", "outputevents")) %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        {
          if (!is.null(stay_id))
            filter(., stay_id == !!stay_id)
          else
            .
        } %>%
        inner_join(d_items, by = "itemid") %>%
        transmute(
            subject_id,
            hadm_id,
            stay_id,
            itemid,
            label,
            abbreviation,
            linksto,
            category,
            unitname,
            param_type,
            lownormalvalue,
            highnormalvalue,
            value = (value),
            valueuom,
            starttime = as.POSIXct(charttime),
            endtime = as.POSIXct(NULL)
        )

      datetimeevents <-
        dplyr::tbl(database, in_schema("mimiciv_icu", "datetimeevents"))  %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        {
          if (!is.null(stay_id))
            filter(., stay_id == !!stay_id)
          else
            .
        } %>%
        inner_join(d_items, by = "itemid") %>%
        transmute(
            subject_id,
            hadm_id,
            stay_id,
            itemid,
            label,
            abbreviation,
            linksto,
            category,
            unitname,
            param_type,
            lownormalvalue,
            highnormalvalue,
            value = NULL,
            valueuom,
            starttime = as.POSIXct(charttime),
            endtime = as.POSIXct(NULL)
        )

      chartevents <-
        dplyr::tbl(database, in_schema("mimiciv_icu", "chartevents")) %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        {
          if (!is.null(stay_id))
            filter(., stay_id == !!stay_id)
          else
            .
        } %>%
        inner_join(d_items, by = "itemid") %>%
        transmute(
            subject_id,
            hadm_id,
            stay_id,
            itemid,
            label,
            abbreviation,
            linksto,
            category,
            unitname,
            param_type,
            lownormalvalue,
            highnormalvalue,
            value = (valuenum),
            valueuom,
            starttime = as.POSIXct(charttime),
            endtime = as.POSIXct(NULL)
        )


      labevents <-
        admissions %>%
        filter(hadm_id == !!hadm_id) %>%
        cross_join(dplyr::tbl(database, in_schema("mimiciv_hosp", "labevents")) %>% filter(subject_id == !!subject_id)) %>%
        filter(sql("charttime > admittime - INTERVAL '2 days' AND charttime < dischtime + INTERVAL '2 days'")) %>%
        inner_join(d_labitems, by = "itemid") %>%
        transmute(
          subject_id = subject_id.x,
          hadm_id = hadm_id.x,
          stay_id = NULL,
          itemid,
          label,
          abbreviation = NULL,
          linksto = "labevents",
          category,
          unitname = NULL,
          param_type = fluid,
          lownormalvalue = NULL,
          highnormalvalue = NULL,
          value = (valuenum),
          valueuom,
          starttime = as.POSIXct(charttime),
          endtime = as.POSIXct(NULL)
        )


      prescriptions <-
        dplyr::tbl(database, in_schema("mimiciv_hosp", "prescriptions")) %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        inner_join(d_prescriptions, by = "drug") %>%
        transmute(
          subject_id,
          hadm_id,
          stay_id = NULL,
          itemid,
          label = drug,
          abbreviation = NULL,
          linksto = "prescriptions",
          category = "Prescriptions",
          unitname = dose_unit_rx,
          param_type = drug_type,
          lownormalvalue = NULL,
          highnormalvalue = NULL,
          value = dose_val_rx,
          valueuom = dose_unit_rx,
          starttime = as.POSIXct(starttime),
          endtime = as.POSIXct(stoptime)
        )

      microbiologyevents <-
        admissions %>%
        filter(hadm_id == !!hadm_id) %>%
        cross_join(dplyr::tbl(database, in_schema("mimiciv_hosp", "microbiologyevents")) %>% filter(subject_id == !!subject_id)) %>%
        filter(sql("charttime > admittime - INTERVAL '2 days' AND charttime < dischtime + INTERVAL '2 days'")) %>%
          transmute(subject_id = subject_id.x,
                    hadm_id = hadm_id.x,
                   stay_id = NULL,
                   itemid = test_itemid,
                   label= test_name,
                   abbreviation = NULL,
                   linksto = "microbiologyevents",
                   category = "Microbiology",
                   unitname = as.character(NULL),
                   param_type = as.character(NULL),
                   lownormalvalue = NULL,
                   highnormalvalue = NULL,
                   value=org_name,
                   valueuom=as.character(microevent_id),
                   starttime=as.POSIXct(charttime),
                   endtime=as.POSIXct(NULL)
          )


      customevents <-
        dplyr::tbl(database, in_schema("public", "customevents")) %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        {
          if (!is.null(stay_id))
            filter(., stay_id == !!stay_id || is.na(stay_id))
          else
            .
        } %>%
        inner_join(d_customevents, by = "itemid") %>%
        transmute(
          subject_id,
          hadm_id,
          stay_id,
          itemid,
          label,
          abbreviation,
          linksto = "customevents",
          category = "User imported",
          unitname = NULL,
          param_type = abbreviation,
          lownormalvalue = NULL,
          highnormalvalue = NULL,
          value = value,
          valueuom,
          starttime = as.POSIXct(charttime),
          endtime = as.POSIXct(NULL)
        )

      demographics <-
        dplyr::tbl(database, in_schema("public", "demographics")) %>%
        filter(subject_id == !!subject_id &
                 hadm_id == !!hadm_id) %>%
        {
          if (!is.null(stay_id))
            filter(., stay_id == !!stay_id || is.na(stay_id))
          else
            .
        } %>%
        transmute(
          subject_id,
          hadm_id,
          stay_id,
          itemid,
          label,
          abbreviation = "",
          linksto = "demographics",
          category = "Demographics",
          unitname = NULL,
          param_type = valueuom,
          lownormalvalue = NULL,
          highnormalvalue = NULL,
          value = value,
          valueuom,
          starttime = as.POSIXct(charttime),
          endtime = as.POSIXct(NULL)
        )

      start.time <- Sys.time()

      from_db_numeric_events <-
        Reduce(
          union_all,
          list(
            procedureevents,
            ingredientevents,
            inputevents,
            outputevents,
            datetimeevents,
            chartevents,
            labevents
          )
        ) %>%
        mutate(value = as.character(value)) %>%
        arrange(starttime) %>%
        collect()


      from_db_character_events <- microbiologyevents %>%
        union(customevents) %>%
        union(prescriptions) %>%
        union(demographics) %>%
        arrange(starttime) %>%
        collect()

      bind_rows(from_db_numeric_events,from_db_character_events)


    } else{
      # Return NULL - to be implemented
      NULL
    }
  }


process_icd_filter <- function(database,user_condition_object,subset_to_filter){
  allow_strings <- unlist(strsplit(user_condition_object$icd_to_allow, "[ ,]"))
  deny_strings <- unlist(strsplit(user_condition_object$icd_to_deny, "[ ,]"))

  query_icd_allow <- tibble(
    icd_code = allow_strings[c(FALSE, TRUE)],
    icd_version = allow_strings[c(TRUE, FALSE)]
  )
  query_icd_deny <- tibble(
    icd_code = deny_strings[c(FALSE, TRUE)],
    icd_version = deny_strings[c(TRUE, FALSE)]
  )

  if(!all(is.na(query_icd_allow))){
    icd_filter_allow <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "diagnoses_icd")) %>%
      {
        if(user_condition_object$allow_condition == "OR"){
          cross_join(.,query_icd_allow,suffix = c(".a", ".b"),copy=TRUE) %>%
            collect() %>%
            filter(startsWith(icd_code.a, icd_code.b) & icd_version.a == icd_version.b)
        } else{
          cross_join(.,query_icd_allow,suffix = c(".a", ".b"),copy=TRUE) %>%
            collect() %>%
            filter(startsWith(icd_code.a, icd_code.b) & icd_version.a == icd_version.b) %>%
            group_by(subject_id,hadm_id) %>%
            summarise(match_count = n()) %>%
            filter(match_count == nrow(query_icd_allow))
        }
      } %>%
      select(subject_id,hadm_id)
  }

  if(!all(is.na(query_icd_deny))){
    icd_filter_deny <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "diagnoses_icd")) %>%
      {
        if(user_condition_object$deny_condition == "OR"){
          cross_join(.,query_icd_deny,suffix = c(".a", ".b"),copy=TRUE) %>%
            collect() %>%
            filter(startsWith(icd_code.a, icd_code.b) & icd_version.a == icd_version.b)
        } else{
          cross_join(.,query_icd_deny,suffix = c(".a", ".b"),copy=TRUE) %>%
            collect() %>%
            filter(startsWith(icd_code.a, icd_code.b) & icd_version.a == icd_version.b)
          group_by(subject_id,hadm_id) %>%
            summarise(match_count = n()) %>%
            filter(match_count == nrow(query_icd_deny))
        }
      } %>%
      select(subject_id,hadm_id)
  }



  icd_filtered_result <- subset_to_filter %>% {
    if(!all(is.na(query_icd_allow))){
      inner_join(.,icd_filter_allow,by=c("subject_id","hadm_id"))
    } else{
      .
    }

  } %>%
    {
      if(!all(is.na(query_icd_deny))){
        anti_join(.,icd_filter_deny,by=c("subject_id","hadm_id"))
      } else{
        .
      }
    }
  icd_filtered_result
}

