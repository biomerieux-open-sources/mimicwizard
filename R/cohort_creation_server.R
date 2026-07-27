cohortCreationServer <- function(id, database = NULL) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 if (!is.null(database)) {

                   selected_profile <- session$userData$selected_profile

                   # State passed to the eventSearchbar to hydrate its DOM when
                   # a saved cohort configuration is loaded (upload / duplicate / edit).
                   preload_state <- reactiveVal(NULL)

                   # If non-NULL, the persist action performs an UPDATE on the given
                   # cohort_id (edit mode) instead of an INSERT of a new cohort.
                   edit_cohort_id  <- reactiveVal(NULL)
                   edit_cohort_src <- reactiveVal(NULL)  # source metadata (name/desc)

                   condition_object <- eventSearchbarServer("event_searchbar", database,
                                                            preload_state = preload_state,
                                                            upload_input_id = ns("upload_config"),
                                                            upload_button_label = "Load saved config")

                   # Cached raw client JSON of the last known filter chain, used to
                   # serialize the current configuration for download / persist.
                   last_filter_tojson <- reactiveVal(NULL)
                   observe({
                     val <- input[["event_searchbar-filter_tojson"]]
                     if (!is.null(val)) last_filter_tojson(val)
                   })

                   # Per-condition SQL captured from the promise resolution to feed
                   # the "Associated SQL" accordion.
                   associated_sql_data <- reactiveVal(NULL)

                   reset_creation_state <- function(reset_ui = FALSE) {
                     fetched_cohort(NULL)
                     edit_cohort_id(NULL)
                     edit_cohort_src(NULL)
                     associated_sql_data(NULL)
                     preload_state(NULL)
                     last_filter_tojson(NULL)

                     if (isTRUE(reset_ui)) {
                       runjs(paste0(
                         "Shiny.setInputValue('",
                         ns("event_searchbar-reset_searchbar_request"),
                         "', Date.now(), {priority: 'event'});"
                       ))
                     }
                   }

                   last_profile_id <- reactiveVal(NULL)
                   observe({
                     profile <- selected_profile()
                     if (is.null(profile) || is.null(profile$user_id)) return()
                     current_id <- suppressWarnings(as.numeric(profile$user_id))
                     previous_id <- last_profile_id()
                     if (is.null(previous_id)) {
                       last_profile_id(current_id)
                       return()
                     }
                     if (!identical(previous_id, current_id)) {
                       reset_creation_state(reset_ui = TRUE)
                       last_profile_id(current_id)
                     }
                   })


                   # ************************************************************************************#
                   #----------------------------- LOAD CONFIGURATION FROM FILE ----------------------------
                   # ____________________________________________________________________________________#

                   observeEvent(input$upload_config, {
                     f <- input$upload_config
                     if (is.null(f)) return()
                     tryCatch({
                       json_text <- paste(readLines(f$datapath, warn = FALSE), collapse = "\n")
                       cfg <- cohort_config_from_json(json_text)
                       preload_state(NULL)
                       preload_state(cfg)
                       edit_cohort_id(NULL)
                       edit_cohort_src(NULL)
                       associated_sql_data(NULL)
                       toast("Configuration loaded",
                             "The search bar has been pre-filled from the uploaded configuration.",
                             "green")
                     }, error = function(e) {
                       toast("Invalid configuration file",
                             htmlEscape(conditionMessage(e)),
                             "red")
                     }, finally = {
                       runjs(paste0(
                         "var fi = document.getElementById('", ns("upload_config"), "');",
                         "if (fi) { fi.value = ''; }"
                       ))
                     })
                   })


                   # ************************************************************************************#
                   #------------------------------- HANDLE EXPLORER PRELOAD -------------------------------
                   # Observe session-scoped signal set by the Cohort Explorer's Edit /
                   # Duplicate icons.
                   # ____________________________________________________________________________________#

                   if (is.null(session$userData$cohort_creation_preload)) {
                     session$userData$cohort_creation_preload <- reactiveVal(NULL)
                   }
                   observe({
                     payload <- session$userData$cohort_creation_preload()
                     if (is.null(payload)) return()
                     preload_state(NULL)
                     preload_state(payload$state)
                     if (identical(payload$mode, "edit")) {
                       edit_cohort_id(payload$source_cohort_id)
                       edit_cohort_src(list(name = payload$source_name,
                                            description = payload$source_desc))
                     } else {
                       edit_cohort_id(NULL)
                       edit_cohort_src(NULL)
                     }
                     associated_sql_data(NULL)
                     # Consume the signal to avoid re-firing on every observe pass.
                     session$userData$cohort_creation_preload(NULL)
                   })


                   # ************************************************************************************#
                   #------------------------------- MODE INDICATOR (EDIT/NEW) -----------------------------
                   # ____________________________________________________________________________________#

                   output$mode_indicator <- renderUI({
                     if (!is.null(edit_cohort_id())) {
                       src <- edit_cohort_src()
                       name <- if (!is.null(src)) src$name else ""
                       message_box(
                         "Editing existing cohort",
                         HTML(paste0(
                           "You are editing cohort <b>", htmlEscape(name),
                           "</b> (ID ", edit_cohort_id(),
                           "). Submitting will overwrite its patient list and configuration."
                         )),
                         class = "warning my-10",
                         closable = FALSE
                       )
                     }
                   })


                   # ************************************************************************************#
                   #------------------------------- FETCH COHORT SUMMARY ----------------------------------
                   # ____________________________________________________________________________________#


                   fetched_cohort <- reactiveVal(NULL)

                   # Keep heavy query execution bound to fresh filter_tojson payloads
                   # produced by explicit Fetch clicks in non-realtime mode.
                   cohort_details_ui <- eventReactive(input[["event_searchbar-filter_tojson"]], {
                     user_condition_object <- condition_object()
                     if (!is.null(user_condition_object) && is.list(user_condition_object) && length(user_condition_object) != 0) {
                       runjs(paste0(
                         "$('#", ns("event_searchbar"), "-filter-container').find('.ui.label.filter').each(function(index) {",
                         "       $(this).removeClass('event-error');",
                         "});"
                       ))
                       # Progress 0-1 init 1-4 request, 4-5 apply logic, 5-6 compute details
                       progress <- Progress$new(session, min = 0, max = 7)
                       progress$set(message = 'Generating request')
                       progress$set(value = 1, message = 'Requesting database')
                       print("Step 1 Generate")
                       promise_list <-
                         request_constrained(user_condition_object, progress)
                       print("Step 2 Resolve")
                       settled_list <- lapply(names(promise_list), function(key) {
                         promise_list[[key]] %>%
                           then(
                             onFulfilled = function(val) list(key = key, status = "ok", value = val),
                             onRejected  = function(e)   list(key = key, status = "error", error = conditionMessage(e))
                           )
                       })
                       names(settled_list) <- names(promise_list)
                       promise_all(.list = settled_list)  %...>% (function(results) {
                       print("Step 3 All is resolved")

                         errors <- Filter(function(r) r$status == "error", results)

                         if (length(errors) > 0) {

                           failed_details <- paste(
                             sapply(errors, function(r) paste0("[", r$key, "] ", r$error)),
                             collapse = "<br>"
                           )

                           # Build a JS array of error indices (extracted from r$key)
                           error_indices <- paste0(
                             "[",
                             paste(
                               sapply(errors, function(r) as.integer(strsplit(r$key, "_")[[1]][2])),
                               collapse = ","
                             ),
                             "]"
                           )

                           progress$close()

                           runjs(paste0(
                             "let errIdx = ", error_indices, ";",
                             "$('#", ns("event_searchbar"), "-filter-container').find('.ui.label.filter').each(function(index) {",
                             "   if (errIdx.includes(index + 1)) {",
                             "       $(this).addClass('event-error');",
                             "   }",
                             "});"
                           ))

                           return(message_box(
                             "An error has occured",
                             HTML(paste0("Are you sure every event has a set and meaningful condition ? That all your parenthesis are correct ? <br> The following query conditions failed:<br><code>", failed_details, "</code>")),
                             class = "negative my-10",
                             closable = TRUE
                           ))
                         }

                         data <- lapply(results, function(r) r$value)
                         names(data) <- sapply(results, function(r) r$key)

                         # Capture per-condition SQL (surfaced via attr() on each
                         # data frame by get_constrained_table).
                         sql_by_condition <- lapply(names(data), function(key) {
                           list(
                             sql = attr(data[[key]], "sql"),
                             params = attr(data[[key]], "params")
                           )
                         })
                         names(sql_by_condition) <- names(data)
                         associated_sql_data(list(
                           per_condition = sql_by_condition,
                           condition_object = user_condition_object
                         ))

                         start.time <- Sys.time()
                         # Check if any condition uses exclusion
                         has_exclusion <- any(sapply(user_condition_object$constraint_list, function(c) isTRUE(c$constraint$is_exclusion)))
                         universe <- NULL
                         if (has_exclusion) {
                           universe <- dplyr::tbl(database(), in_schema("public", "demographics")) %>%
                             select(subject_id, hadm_id, stay_id) %>%
                             distinct() %>%
                             collect()
                         }
                         for(key in names(data)){
                           condition_idx <- gsub("condition_", "", key)
                           d <- data[[key]]
                           if (isTRUE(user_condition_object$constraint_list[[condition_idx]]$constraint$is_exclusion)) {
                             # Pre-compute complement: stays in universe NOT matching this condition
                             join_by <- if (!("stay_id" %in% names(d)) || is.null(d$stay_id[1])) {
                               c("subject_id", "hadm_id")
                             } else {
                               c("subject_id", "hadm_id", "stay_id")
                             }
                             d <- anti_join(universe, d, by = join_by)
                           }
                           assign(key, d)
                         }
                         escaped_expression <- parsecondition(user_condition_object$condition_string)
                         expression_to_eval <- gsub("[","(",escaped_expression,fixed=T)
                         expression_to_eval <- gsub("]",")",expression_to_eval,fixed=T)
                         progress$set(value = 4, message = 'Applying logic between filters')
                         result <- eval(parse(text=expression_to_eval))
                         progress$set(value = 5, message = 'Applying ICD filters')
                         icd_filtered_result <- process_icd_filter(database,user_condition_object,result)

                         admissions <-
                           dplyr::tbl(database(), in_schema("mimiciv_hosp", "admissions"))
                         selected_cohort <-
                           dplyr::tbl(database(), in_schema("mimiciv_hosp", "patients")) %>%
                           inner_join(icd_filtered_result, copy = TRUE) %>%
                           inner_join(admissions, by = c("subject_id","hadm_id")) %>%
                           select(subject_id,
                                  hadm_id,
                                  stay_id,
                                  gender,
                                  anchor_age,
                                  admittime,
                                  anchor_year) %>%
                           collect()
                         end.time <- Sys.time()
                         time.taken <- end.time - start.time
                         print(paste("Process result time :",time.taken))
                         progress$set(value = 6, message = 'Computing stats about the fetched cohort')


                         count_subject <-
                           selected_cohort %>%  select(subject_id) %>% distinct() %>% count() %>% rename("Total subjects" = n) %>% collect()
                         count_hadm <-
                           selected_cohort %>%  select(hadm_id) %>% distinct() %>% count() %>% rename("Total hadm" = n) %>% collect()
                         count_stay <-
                           selected_cohort %>%  select(stay_id) %>% distinct() %>% count() %>% rename("Total stays" = n) %>% collect()
                         count_subject_with_one_icu <-
                           nrow(
                             selected_cohort  %>% select(subject_id, stay_id) %>% group_by(subject_id) %>% count() %>% filter(n >
                                                                                                                                0) %>% collect()
                           )

                         ratio_subject_with_one_icu <-
                           list("Subject with at least one ICU Stay" =
                                  paste0((count_subject_with_one_icu / count_subject) * 100,
                                         "%"
                                  ))

                         subject_list <- selected_cohort

                         sex_count <-
                           subject_list %>% select(subject_id, gender) %>% distinct() %>% group_by(gender) %>% count() %>% collect() %>% pivot_wider(names_from = gender, values_from = n) %>% mutate_if(is.numeric, as.integer)

                         if (length(sex_count) == 2) {
                           sex_ratio = list("Sex ratio" = paste0(
                             round(sex_count[["M"]] / sex_count[["F"]], 3),
                             " (",
                             sex_count[["M"]],
                             "M",
                             " ",
                             sex_count[["F"]],
                             "F",
                             ")"
                           ))
                         } else{
                           if (nrow(sex_count) > 0) {
                             if (names(sex_count)[1] == "M") {
                               sex_ratio = list("Sex ratio" = paste0("1 (", sex_count[names(sex_count)[1]], names(sex_count)[1], ")"))
                             } else{
                               sex_ratio = list("Sex ratio" = paste0("0 (", sex_count[names(sex_count)[1]], names(sex_count)[1], ")"))
                             }
                           }
                           else{
                             sex_ratio = list("Sex ratio" = paste0("0M/0F"))
                           }


                         }

                         age_mean_median <- subject_list %>%
                           mutate(age = anchor_age + as.numeric(format(admittime, format =
                                                                         "%Y")) - anchor_year) %>%
                           summarise(mean_age = mean(age),
                                     median_age = median(age)) %>% mutate(mean_age = round(mean_age, 2)) %>% rename("Mean age" = mean_age, "Median age" = median_age)

                         summary <-
                           bind_cols(
                             count_subject,
                             count_hadm,
                             count_stay,
                             ratio_subject_with_one_icu,
                             sex_ratio,
                             age_mean_median
                           )

                         fetched_cohort(selected_cohort %>% select(subject_id, hadm_id, stay_id))
                         progress$set(message = 'Cohort successfully fetched',
                                      value = 6)
                         accordion_content <-
                           list(list(title = "Cohort Summary",
                                     content = tagList(
                                       lapply(seq_along(summary), function(i) {
                                         div(tags$b(paste0(names(
                                           summary
                                         )[[i]], " : ")) , tags$span(summary[[i]]))
                                       })
                                     )))

                         progress$close()
                         accordion(accordion_content,
                                   active_title = "Cohort Summary",
                                   fluid = TRUE)
                       })
                     }
                     else{
                       fetched_cohort(NULL)
                       associated_sql_data(NULL)
                       NULL
                     }
                   })

                   output$fetched_details <- renderUI({
                     cohort_details_ui()
                   })


                   # ************************************************************************************#
                   #------------------------------- ASSOCIATED SQL ACCORDION ------------------------------
                   # ____________________________________________________________________________________#

                   output$associated_sql <- renderUI({
                     data <- associated_sql_data()
                     if (is.null(data)) return(NULL)
                     render_associated_sql_accordion(data$per_condition, data$condition_object)
                   })


                   # ************************************************************************************#
                   #---------------------------------- PERSIST COHORT ------------------------------------
                   # ____________________________________________________________________________________#


                   output$persist_action <- renderUI({

                     if (!is.null(fetched_cohort())) {
                       btn_label <- if (!is.null(edit_cohort_id())) "Update Cohort" else "Persist Cohort"
                       src <- edit_cohort_src()
                       init_name <- if (!is.null(src)) src$name        else ""
                       init_desc <- if (!is.null(src)) src$description else ""

                       htmltools::tagAppendAttributes(form(
                         fields(
                           field(
                             tags$label("Enter the cohort name"),
                             text_input(
                               ns("cohort_name"),
                               label = "",
                               value = init_name
                             ),
                             class = "four wide"
                           ),

                           field(
                             tags$label("Enter the cohort description"),
                             text_input(
                               ns("cohort_desc"),
                               label = "",
                               value = init_desc
                             ),
                             class = "twelve wide"
                           )
                           ,
                           class = "inline"
                         ),
                         button(ns("persist_button"), label = btn_label),
                         downloadButton(ns("download_config"), "Download configuration (JSON)",
                                        class = "ui button", icon = NULL),
                         class = "mt-10"
                       ),style="position:static")
                     }
                   })

                   output$download_config <- downloadHandler(
                     filename = function() {
                       name_part <- if (isTruthy(input$cohort_name)) {
                         gsub("[^A-Za-z0-9._-]+", "_", input$cohort_name)
                       } else {
                         "cohort_config"
                       }
                       paste0("mimicwizard_cohort_", name_part, "_",
                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
                     },
                     content = function(file) {
                       cfg <- cohort_config_from_client(
                         filter_tojson_raw = last_filter_tojson(),
                         icd_to_keep = input[["event_searchbar-icd_to_keep"]],
                         icd_to_deny = input[["event_searchbar-icd_to_deny"]],
                         allow_condition = input[["event_searchbar-icd_to_keep_condition"]],
                         deny_condition  = input[["event_searchbar-icd_to_deny_condition"]],
                         cohort_name = if (isTruthy(input$cohort_name)) input$cohort_name else NULL,
                         cohort_description = if (isTruthy(input$cohort_desc)) input$cohort_desc else NULL
                       )
                       writeLines(cohort_config_to_json(cfg), file)
                     }
                   )

                   persist_click_key <- paste0(ns("persist_button"), "_last_count")
                   if (is.null(session$userData[[persist_click_key]])) {
                     session$userData[[persist_click_key]] <- reactiveVal(0L)
                   }
                   last_persist_click <- session$userData[[persist_click_key]]

                   persistMessage <-
                     eventReactive(input$persist_button, {
                       current_click <- if (is.null(input$persist_button)) 0L else as.integer(input$persist_button)
                       if (current_click <= isolate(last_persist_click())) {
                         return(invisible(NULL))
                       }
                       last_persist_click(current_click)

                       if (input$cohort_name == "" || input$cohort_desc == "" || is.null(fetched_cohort())) {
                         toast("",
                               paste0("A cohort should have a non-empty name and description"),
                               "red")
                         return(invisible(NULL))
                       }

                       # Build the JSON configuration to persist alongside the cohort row.
                       cfg <- cohort_config_from_client(
                         filter_tojson_raw = last_filter_tojson(),
                         icd_to_keep = input[["event_searchbar-icd_to_keep"]],
                         icd_to_deny = input[["event_searchbar-icd_to_deny"]],
                         allow_condition = input[["event_searchbar-icd_to_keep_condition"]],
                         deny_condition  = input[["event_searchbar-icd_to_deny_condition"]],
                         cohort_name = input$cohort_name,
                         cohort_description = input$cohort_desc
                       )
                       cfg_json <- as.character(cohort_config_to_json(cfg, pretty = FALSE))

                       editing_id <- edit_cohort_id()

                       if (is.null(editing_id)) {
                         persist_new_cohort(database, input$cohort_name, input$cohort_desc,
                                            fetched_cohort(), cfg_json, selected_profile)
                       } else {
                         persist_update_cohort(database, editing_id,
                                               input$cohort_name, input$cohort_desc,
                                               fetched_cohort(), cfg_json)
                       }
                     })

                   output$result_persist <- renderUI({
                     msg <- persistMessage()
                     if (!is.null(msg)) {
                       # After a successful persist/update, fully clear creation state.
                       reset_creation_state(reset_ui = TRUE)
                     }
                     msg
                   })


                   }
})
}


# ------------------------------------------------------------------------------
# Persist helpers
# ------------------------------------------------------------------------------

persist_new_cohort <- function(database, cohort_name, cohort_description,
                               cohort_rows, cfg_json, selected_profile) {
  withProgress(message = "Checking if cohort name is unique", {
    cohort_names <-
      dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>%
      select("cohort_name") %>% collect()
    if (cohort_name %in% as.list(cohort_names)$cohort_name) {
      toast("",
            paste0(
              "A cohort named \"",
              htmlEscape(cohort_name),
              "\" already exist in database. Cohort name should be unique"
            ),
            "yellow")
      return(NULL)
    }

    setProgress(value = 1 / 5, message = "Cohort description registration")
    insert_q <- "INSERT INTO public.d_cohorts (cohort_name, cohort_description, cohort_definition) VALUES ($1, $2, $3::jsonb) RETURNING cohort_id"
    rs <- DBI::dbSendQuery(database(), insert_q)
    DBI::dbBind(rs, list(cohort_name, cohort_description, cfg_json))
    inserted <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    new_cohort_id <- as.numeric(inserted$cohort_id[1])

    setProgress(value = 3 / 5, message = "Retrieving data from cache")
    cohort_data <- cohort_rows %>%
      select(subject_id, hadm_id, stay_id) %>%
      mutate(cohort_id = new_cohort_id)

    setProgress(value = 4 / 5, message = "Cohort data persisting")
    dbAppendTable(database(),
                  Id(schema = "public", table = "cohort"),
                  as.data.frame(cohort_data))

    if (!is.null(selected_profile) && selected_profile()$user_id != 0) {
      query <- "UPDATE users SET user_cohorts = user_cohorts || $1 WHERE user_id = $2"
      update <- dbSendQuery(database(), query)
      dbBind(update, list(paste0(',', new_cohort_id), selected_profile()$user_id))
      dbClearResult(update)
    }

    toast("",
          paste0("Your cohort <b>", htmlEscape(cohort_name),
                 "</b> has been persisted"),
          "green")
    message_box(
      "Persist action success",
      "Your now able to explore your cohort in Cohort Explorer page",
      class = "positive my-10",
      closable = TRUE
    )
  })
}


persist_update_cohort <- function(database, cohort_id, cohort_name,
                                  cohort_description, cohort_rows, cfg_json) {
  withProgress(message = "Updating cohort", value = 0, {
    db <- database()
    tryCatch({
      DBI::dbBegin(db)

      # Ensure name uniqueness among *other* cohorts.
      setProgress(value = 1 / 5, message = "Validating cohort name")
      name_conflict <- dplyr::tbl(db, in_schema("public", "d_cohorts")) %>%
        filter(cohort_name == !!cohort_name & cohort_id != !!cohort_id) %>%
        count() %>% collect()
      if (as.numeric(name_conflict$n[1]) > 0) {
        DBI::dbRollback(db)
        toast("",
              paste0("A cohort named \"", htmlEscape(cohort_name),
                     "\" already exist in database. Cohort name should be unique"),
              "yellow")
        return(NULL)
      }

      setProgress(value = 2 / 5, message = "Updating cohort description")
      upd <- "UPDATE public.d_cohorts SET cohort_name = $1, cohort_description = $2, cohort_definition = $3::jsonb WHERE cohort_id = $4"
      rs <- DBI::dbSendQuery(db, upd)
      DBI::dbBind(rs, list(cohort_name, cohort_description, cfg_json, cohort_id))
      DBI::dbClearResult(rs)

      setProgress(value = 3 / 5, message = "Removing previous cohort stays")
      del <- "DELETE FROM public.cohort WHERE cohort_id = $1"
      rs <- DBI::dbSendQuery(db, del)
      DBI::dbBind(rs, list(cohort_id))
      DBI::dbClearResult(rs)

      setProgress(value = 4 / 5, message = "Inserting refreshed cohort stays")
      new_rows <- cohort_rows %>%
        select(subject_id, hadm_id, stay_id) %>%
        mutate(cohort_id = cohort_id)
      dbAppendTable(db,
                    Id(schema = "public", table = "cohort"),
                    as.data.frame(new_rows))

      DBI::dbCommit(db)
    }, error = function(e) {
      tryCatch(DBI::dbRollback(db), error = function(e2) NULL)
      toast("Update failed", htmlEscape(conditionMessage(e)), "red")
      return(NULL)
    })

    toast("",
          paste0("Cohort <b>", htmlEscape(cohort_name), "</b> has been updated"),
          "green")
    message_box(
      "Update success",
      "The cohort configuration and patient list have been overwritten.",
      class = "positive my-10",
      closable = TRUE
    )
  })
}


# ------------------------------------------------------------------------------
# Associated SQL accordion renderer
# ------------------------------------------------------------------------------

render_associated_sql_accordion <- function(per_condition, condition_object) {
  content <- render_associated_sql_content(per_condition, condition_object)
  accordion(list(list(title = "Associated SQL", content = content)),
            active_title = NULL,
            fluid = TRUE)
}



