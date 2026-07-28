cohortExplorerServer <- function(id,
                                 database = NULL,
                                 selected_profile = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    if (!is.null(database)) {
      selected_profile <- session$userData$selected_profile

      cohortOutcomesExplorerServer("cohort_outcomes_explorer",database,isolate(reactive(input$cohort_picker)))

      cohort_restriction <- reactive({
        input$update_cohort_picker
        profile <- selected_profile()
        uid <- suppressWarnings(as.numeric(profile$user_id))
        if (is.na(uid) || uid == 0) {
          # Admin profile: no restriction (show all cohorts).
          return(numeric(0))
        }

        # Refresh permissions locally from DB to avoid global update_profile churn.
        q <- DBI::dbSendQuery(database(), "SELECT user_cohorts FROM users WHERE user_id = $1")
        on.exit(try(DBI::dbClearResult(q), silent = TRUE), add = TRUE)
        DBI::dbBind(q, list(uid))
        row <- DBI::dbFetch(q)

        if (nrow(row) == 0 || is.null(row$user_cohorts[1]) || is.na(row$user_cohorts[1])) {
          return(numeric(0))
        }

        vals <- unlist(strsplit(as.character(row$user_cohorts[1]), ",", fixed = TRUE))
        vals <- trimws(vals)
        ids <- suppressWarnings(as.numeric(vals[nzchar(vals)]))
        ids[!is.na(ids)]
      })
      timeline_itemid <- reactive({
        selected_profile
        selected_profile()$user_itemids
      })
      cards_itemid <- reactive({
        selected_profile
        selected_profile()$user_itemids
      })

      output$cohort_picker_ui <-
        renderUI({
          input$update_cohort_picker
          d_cohorts <-
            dplyr::tbl(database(), in_schema("public", "d_cohorts"))
          if (length(cohort_restriction()) == 0) {
            cohort_list <- d_cohorts %>% collect()
          } else {
            cohort_list <-
              d_cohorts %>% filter(cohort_id %in% !!cohort_restriction()) %>% collect()
          }
          tagList(
            htmltools::tagAppendAttributes(
              dropdown_input(
                ns("cohort_picker"),
                default_text = "Pick a cohort",
                choices_value = cohort_list$cohort_id,
                choices = cohort_list$cohort_name
              ),
              style = "width: 270px !important;display:inline-block;"
            ),
            tags$div(
              tagList(
                icon("pointer grey sync rotate-on-hover", id = "refresh-cohort-picker"),
                icon("pointer-scale grey pencil", id = "edit-cohort-picker"),
                icon("pointer-scale grey clone", id = "duplicate-cohort-picker"),
                downloadLink(
                  ns("download_cohort_config"),
                  label = icon("pointer-scale grey download"),
                  class = "cohort-download-config-link"
                ),
                icon("pointer-scale grey trash", id = "delete-cohort-picker")
                ),style="width:150px;display:inline-flex;justify-content:space-around;"
              )
          )
        })

      observeEvent(input[["delete_cohort"]], {
        if(input$cohort_picker!= ""){
          create_modal(modal(
            id = ns("delete-cohort-modal"),
            header = h2("Delete a cohort"),
            content = paste0("Are you sure you want to delete the selected cohort ?"),
            footer = tagList(
              button(ns("cancel_delete_cohort"), label = "Cancel"),
              button(
                ns("confirm_delete_cohort"),
                label = "Confirm delete",
                class = "red"
              )
            )
          ))
          show_modal(ns("delete-cohort-modal"))
        }

      })
      observeEvent(input[["cancel_delete_cohort"]], {
        hide_modal(ns("delete-cohort-modal"))
      })
      observeEvent(input[["confirm_delete_cohort"]], {
        if(!is.na(as.numeric(input$cohort_picker))){
          runjs(paste0("document.getElementById('",ns("confirm_delete_cohort"),"').innerHTML = '<i class=\"notched circle loading icon\"></i>';"))
          query <- "DELETE FROM public.d_cohorts WHERE cohort_id = $1"
          delete <- dbSendQuery(database(), query)

          dbBind(delete, list(input$cohort_picker))
          dbClearResult(delete)

          hide_modal(ns("delete-cohort-modal"))
          runjs(paste0("document.getElementById('",ns("confirm_delete_cohort"),"').innerHTML = 'Confirm delete';"))
          runjs(paste0("Shiny.setInputValue('",
                                ns('update_cohort_picker'),
                                "', Date.now());"))
        }
      })

      # ****************************************************************************
      # Edit / Duplicate / Download configuration icons on the picker.
      # ****************************************************************************

      fetch_cohort_definition <- function(cohort_id_val) {
        if (!isTruthy(cohort_id_val) || is.na(as.numeric(cohort_id_val))) return(NULL)
        row <- dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>%
          filter(cohort_id == !!as.numeric(cohort_id_val)) %>%
          collect()
        if (nrow(row) == 0) return(NULL)
        cfg <- NULL
        if (!is.null(row$cohort_definition) && !is.na(row$cohort_definition[[1]]) &&
            nzchar(as.character(row$cohort_definition[[1]]))) {
          cfg <- tryCatch(
            cohort_config_from_json(as.character(row$cohort_definition[[1]])),
            error = function(e) NULL
          )
        }
        list(
          row = row,
          cfg = cfg
        )
      }

      trigger_creation_preload <- function(mode) {
        fetched <- fetch_cohort_definition(input$cohort_picker)
        if (is.null(fetched)) {
          toast("Cohort not found",
                "The selected cohort could not be loaded.",
                "red")
          return()
        }
        if (is.null(fetched$cfg)) {
          toast("No configuration saved",
                "This cohort was created before configuration saving was enabled. It cannot be edited or duplicated.",
                "yellow")
          return()
        }

        if (is.null(session$userData$cohort_creation_preload)) {
          session$userData$cohort_creation_preload <- reactiveVal(NULL)
        }
        session$userData$cohort_creation_preload(list(
          state = fetched$cfg,
          mode = mode,
          source_cohort_id = as.numeric(fetched$row$cohort_id[1]),
          source_name = as.character(fetched$row$cohort_name[1]),
          source_desc = as.character(fetched$row$cohort_description[1])
        ))
        # Switch to the Cohort Creation tab.
        session$sendCustomMessage("update_tab", "cohort_creation")
        # Fallback for deployments where the semantic.dashboard handler differs.
        runjs("setTimeout(function(){ var t = document.querySelector('#uisidebar [data-value=\"cohort_creation\"]'); if (t) { t.click(); } }, 0);")
      }

      observeEvent(input[["edit_cohort"]], {
        if (isTruthy(input$cohort_picker)) trigger_creation_preload("edit")
      })
      observeEvent(input[["duplicate_cohort"]], {
        if (isTruthy(input$cohort_picker)) trigger_creation_preload("duplicate")
      })

      output$download_cohort_config <- downloadHandler(
        filename = function() {
          cohort_id_val <- input$cohort_picker
          name_part <- "cohort_config"
          if (isTruthy(cohort_id_val) && !is.na(as.numeric(cohort_id_val))) {
            row <- dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>%
              filter(cohort_id == !!as.numeric(cohort_id_val)) %>%
              select(cohort_name) %>% collect()
            if (nrow(row) > 0 && isTruthy(row$cohort_name[1])) {
              name_part <- gsub("[^A-Za-z0-9._-]+", "_", row$cohort_name[1])
            }
          }
          paste0("mimicwizard_cohort_", name_part, "_",
                 format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
        },
        content = function(file) {
          fetched <- fetch_cohort_definition(input$cohort_picker)
          if (is.null(fetched) || is.null(fetched$cfg)) {
            writeLines("{\"error\": \"No configuration saved for this cohort.\"}", file)
            return()
          }
          # Refresh top-level metadata to reflect the cohort as stored.
          fetched$cfg$cohort_name <- as.character(fetched$row$cohort_name[1])
          fetched$cfg$cohort_description <- as.character(fetched$row$cohort_description[1])
          writeLines(cohort_config_to_json(fetched$cfg), file)
        }
      )

      output$cohort_picked_ui <- renderUI({
        req(input$cohort_picker)
        if (input$cohort_picker != -1) {
          d_cohorts <-
            dplyr::tbl(database(), in_schema("public", "d_cohorts"))
          selected_cohort <-
            d_cohorts %>% filter(cohort_id == !!input$cohort_picker) %>% collect()
          htmltools::tagAppendAttributes(div(
            selected_cohort$cohort_name,
            tags$small(selected_cohort$cohort_description)
          ),
          class = "cohort-description")
        }
      })

      sex_ratio_to_sex_count <-
        function(sex_count) {
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
          sex_ratio
        }


      output$cohort_summary <- renderUI({
        req(input$cohort_picker)
        if (input$cohort_picker != -1) {
          d_cohorts <-
            dplyr::tbl(database(), in_schema("public", "d_cohorts"))
          cohort <-
            dplyr::tbl(database(), in_schema("public", "cohort"))
          patients <-
            dplyr::tbl(database(), in_schema("mimiciv_hosp", "patients"))
          admissions <-
            dplyr::tbl(database(), in_schema("mimiciv_hosp", "admissions"))

          count_subject <-
            cohort %>% filter(cohort_id == !!input$cohort_picker) %>% select(subject_id) %>% distinct() %>% count() %>% rename("Total subjects" = n) %>% collect()
          count_hadm <-
            cohort %>% filter(cohort_id == !!input$cohort_picker) %>% select(hadm_id) %>% distinct() %>% count() %>% rename("Total hadm" = n) %>% collect()
          count_stay <-
            cohort %>% filter(cohort_id == !!input$cohort_picker) %>% select(stay_id) %>% distinct() %>% count() %>% rename("Total stays" = n) %>% collect()
          count_subject_with_one_icu <-
            nrow(
              cohort %>% filter(cohort_id == !!input$cohort_picker) %>% select(subject_id, stay_id) %>% group_by(subject_id) %>% count() %>% filter(n >
                                                                                                                                                      0) %>% collect()
            )

          ratio_subject_with_one_icu = list("Subject with at least one ICU Stay" =
                                              paste0((count_subject_with_one_icu / count_subject) * 100,
                                                     "%"
                                              ))

          subject_list <- cohort %>%
            filter(cohort_id == !!input$cohort_picker) %>%
            inner_join(patients, by = "subject_id")

          sex_count <-
            subject_list %>% select(subject_id, gender) %>% distinct() %>% group_by(gender) %>% count() %>% collect() %>% pivot_wider(names_from = gender, values_from = n) %>% mutate_if(is.numeric, as.integer)

          sex_ratio <-
            sex_ratio_to_sex_count(sex_count)

          age_mean_median <- subject_list %>%
            inner_join(admissions, by = "subject_id") %>%
            mutate(age = anchor_age + DATE_PART('year', admittime) - anchor_year) %>%
            summarise(mean_age = mean(age),
                      median_age = median(age)) %>% mutate(mean_age = round(mean_age, 2)) %>% rename("Mean age" = mean_age, "Median age" = median_age) %>% collect()

          summary <-
            bind_cols(
              count_subject,
              count_hadm,
              count_stay,
              ratio_subject_with_one_icu,
              sex_ratio,
              age_mean_median
            )

          accordion_content <-
            list(list(title = "Cohort Summary", content = tagList(
              lapply(seq_along(summary), function(i) {
                div(tags$b(paste0(names(
                  summary
                )[[i]], " : ")) , tags$span(summary[[i]]))
              })
            )))

          # If a saved configuration exists for this cohort, add an
          # "Associated SQL" accordion computed from the stored definition
          # without hitting MIMIC (query_generator produces the string only).
          sql_section <- tryCatch({
            fetched <- fetch_cohort_definition(input$cohort_picker)
            if (!is.null(fetched$cfg)) {
              cond_obj <- cohort_config_to_condition_object(fetched$cfg, database)
              per_condition <- list()
              cl <- cond_obj$constraint_list
              if (length(cl) > 0) {
                for (i in seq_along(cl)) {
                  key <- paste0("condition_", i)
                  entry <- cl[[i]]
                  per_condition[[key]] <- tryCatch(
                    get_constrained_table_sql(
                      database(),
                      entry$linksto,
                      entry$constraint,
                      NULL
                    ),
                    error = function(e) list(sql = paste("Error:", conditionMessage(e)), params = list())
                  )
                }
              }
              list(list(title = "Associated SQL",
                        content = render_associated_sql_content(per_condition, cond_obj)))
            } else NULL
          }, error = function(e) NULL)

          if (!is.null(sql_section)) {
            accordion_content <- c(accordion_content, sql_section)
          }

          accordion(accordion_content,
                    active_title = "Cohort Summary",
                    fluid = TRUE)
        }

      })

      # Use patient explorer as a module to display information
      observe({
        patientExplorerInstance <- patientExplorerServer("cohort_patient_explorer",
                                                         database,
                                                         input$cohort_picker)
      })

      allowed_target <- list(
        "inputevents" = c("amount", "rate"),
        "chartevents" = c("value", "valuenum"),
        "labevents" = c("value", "valuenum"),
        "ingredientevents" = c("amount", "rate"),
        "outputevents" = c("value"),
        "datetimeevents" = c("value"),
        "procedureevents" = c("value"),
        "microbiologyresultsevents"= c("value","valuenum"),
        "prescriptions" = c("dose_val_rx","doses_per_24_hrs"),
        "customevents" = c("value"),
        "demographics" = c("value")

      )
      target_to_uom <- list(
        "amount" = "amountuom",
        "rate" = "rateuom",
        "value" = "valueuom",
        "valuenum" = "valueuom",
        "dose_val_rx" = "dose_unit_rx"
      )
      event_time_target <- list(
        "inputevents" = "starttime",
        "chartevents" = "charttime",
        "labevents" = "charttime",
        "ingredientevents" = "starttime",
        "outputevents" = "charttime",
        "datetimeevents" = "charttime",
        "procedureevents" = "starttime",
        "microbiologyresultsevents"= "charttime",
        "prescriptions" = "starttime",
        "customevents" = "charttime",
        "demographics" = "charttime"
      )

      distinct_events <-
        reactiveVal(dplyr::tbl(database(), in_schema("public", "distinct_events")) %>% collect())
      output$parameter_picker_ui <- renderUI({
        req(input$cohort_picker)
        if (input$cohort_picker != -1) {
          choices <- distinct_events() %>%
            select(itemid, category, label,param_type) %>%
            pivot_wider(
              names_from = c(label, param_type, category, itemid),
              names_glue = "{category} - {param_type} {label} ({itemid})",
              values_from = itemid
            )

          tagList(
            tags$label("Select the parameter to draw"),
            custom_search_selection_choices(ns("parameter_picker"), choices =
                                              choices)
          )

        }

      })
      output$field_picker_ui <- renderUI({
        req(input$cohort_picker, input$parameter_picker)
        choices <-
          allowed_target[[as.character(
            distinct_events() %>% filter(itemid == !!input$parameter_picker) %>% select(linksto)
          )]]
        selectInput(
          ns("field_picker"),
          label = tagList(
            "Select your target column :",
            icon("inline info circle icon link"),
            tags$div(tags$div(
              HTML(
                "<b>value</b> contains the value measured for the concept identified by the itemid.
                                                      If this value is numeric, then <b>valuenum</b> contains the same data in a numeric format.<br>
                                                      If this data is not numeric, <b>valuenum</b> is null. <br>
                                                      In some cases (e.g. scores like Glasgow Coma Scale), <b>valuenum</b> contains the score and <b>value</b> contains the score and text describing the meaning of the score.<br>
                                                      For labs result, please use the <i class='glasses icon'></i> preview tool in cohort creation tab to have an overview of the data format<br> Source : <a href='https://mimic.mit.edu/docs/iv/modules/icu/chartevents/'>mimic.mit.edu</a>"
              ),
              class = "content"
            ), class = "ui popup right center transition wide hidden")
          ),
          choices = choices
        )
      })


      output$force_cast_ui <- renderUI({
        req(input$cohort_picker, input$parameter_picker)
        checkbox_input(
          ns("force_cast"),
          "Force cast to numeric",
          type="toggle", is_marked=isolate({
            ifelse(isTruthy(input$force_cast),
                   input$force_cast,
                   FALSE)
          }))
      })

      output$fetch_action <- renderUI({
        req(input$cohort_picker, input$field_picker)
        runjs(
          "$('.inline.icon')
                            .popup({
                              inline: true,
                              on: 'click',
                              position: 'bottom center'
                            })
                          ;"
        )
        htmltools::tagAppendAttributes(button(ns("fetch_button"), label = "Fetch Data"), class =
                                         "my-10")
      })

      plot_data <- reactiveVal(NULL)

      observeEvent(input$fetch_button, {
        withProgress(value = 1 / 5, message = "Generating request", {
          table <-
            as.character(
              distinct_events() %>% filter(itemid == !!input$parameter_picker) %>% select(linksto)
            )
          schema <- get_table_schema(table)
          itemid <- input$parameter_picker

          column_type <-
            get_column_type(input$field_picker, schema, table)

          if (column_type == "character varying" && input$force_cast) {
            field <- paste0(
              "CASE WHEN ",
              input$field_picker,
              "~E'^[+-]?([0-9]*[.])?[0-9]+$' THEN ",
              input$field_picker,
              "::numeric ELSE NULL end"
            )
          } else{
            field <- input$field_picker
          }

          if (schema == "mimiciv_icu") {
            query <-
              paste0(
                "SELECT ",
                field,
                " AS value,t.stay_id AS stay_id,CAST(hr AS INTEGER) as hr, t.",
                target_to_uom[[input$field_picker]],
                " as unitname FROM ",
                schema,
                ".",
                table,
                " t
                       JOIN
                       (SELECT stay_id,hr,endtime - interval '1 hour' AS starttime,endtime FROM mimiciv_derived.icustay_hourly ih)  ih
                       ON (t.stay_id = ih.stay_id AND t.",
                event_time_target[[table]],
                " BETWEEN ih.starttime AND ih.endtime)
                       JOIN public.cohort c ON c.stay_id = t.stay_id
                       WHERE itemid = ",
                itemid,
                " AND cohort_id=",
                input$cohort_picker
              )
          } else{
            query <-
              paste0(
                "SELECT ",
                field,
                " AS value,ih.stay_id AS stay_id,CAST(hr AS INTEGER) as hr, t.",
                target_to_uom[[input$field_picker]],
                " as unitname FROM ",
                schema,
                ".",
                table,
                " t
                       JOIN mimiciv_icu.icustays icust ON ( icust.subject_id = t.subject_id AND t.hadm_id = icust.hadm_id AND ",event_time_target[[table]]," <@ tsrange(intime - interval '24 hour', outtime + interval '24 hour'))
                       JOIN
                       (SELECT stay_id,hr,endtime - interval '1 hour' AS starttime,endtime FROM mimiciv_derived.icustay_hourly ih)  ih
                       ON (icust.stay_id = ih.stay_id AND t.",
                event_time_target[[table]],
                " BETWEEN ih.starttime AND ih.endtime)
                       JOIN public.cohort c ON c.stay_id = icust.stay_id ",
                       ifelse(table=="prescriptions","JOIN public.d_prescriptions  USING(drug) ",""),
                       "WHERE itemid = ",
                itemid,
                " AND cohort_id=",
                input$cohort_picker
              )
          }

          print(sql(query))

          setProgress(value = 3 / 5, message = "Requesting database")
          data <- dplyr::tbl(database(), sql(query)) %>% collect()
          if(nrow(data)>100){
            plot_data(data)
          } else if (nrow(data)!=0){
            shiny.semantic::toast(
              message = paste0(
                "Only <b>",length(data),"</b> row fetched<br>
                Is your cohort small or your parameter defined for everyone ?"
              ),
              class = "warning",
              duration = 10
            )

            plot_data(data)
          }else{
            shiny.semantic::toast(
              message = paste0(
                "No row fetched<br>
                This parameter is not defined in this cohort"
              ),
              class = "error",
              duration = 10
            )
            plot_data(NULL)
          }

          setProgress(value = 5 / 5, message = "Data fetched")
        })
      })

      aggr_func <- list("mean", "min", "max", "median", "sum")
      plot_types <- list("Boxplot", "Longitudinal trajectory", "Violin","Pie (categorical)")
      output$plot_config_form <- renderUI({
        req(input$cohort_picker)
        if (isTruthy(plot_data())) {
          min_hour <- min(plot_data()$hr)
          max_hour <- max(plot_data()$hr)

          htmltools::tagAppendAttributes(form({
            if (sum(!is.na(as.numeric(plot_data()$value))) < nrow(plot_data())){
              message_box(
                "Non-numeric data",
                paste0("Some value that you're trying to observe is non numeric or null (",round(sum(is.na(as.numeric(plot_data()$value)))/nrow(plot_data()),4)*100,"%). Some function may return error/empty diagram. Ignore this message if this is expected, otherwise, consider using numeric cast"),
                class = "yellow my-10",
                icon_name = "calculator",
                closable = T
              )
            } else if(sum(!is.na(as.numeric(plot_data()$value))) > nrow(plot_data())*0.5 & typeof(plot_data()$value)=="character"){
              message_box(
                "Non-numeric data",
                paste0("Your using number as characters (",round(sum(!is.na(as.integer(plot_data()$value)))/nrow(plot_data()),2)*100,"%). Some function may return error/empty diagram. Ignore this message if this is expected, otherwise, consider using numeric cast"),
                class = "blue my-10",
                icon_name = "sort numeric up",
                closable = T
              )
            }

          }, fields(
            if(!isTruthy(input$plot_type) || input$plot_type != "Pie (categorical)"){
            tagList(field(
              labeled_numeric_input(
                ns("plot_timestep"),
                label = "Enter diagram time step (h)",
                value = isolate({
                  ifelse(isTruthy(input$plot_timestep),
                         input$plot_timestep,
                         6)
                }),
                min = 1,
                step = 1,
                type = "number"
              )
            ),
            field(
              labeled_numeric_input(
                ns("plot_starttime"),
                label = "Enter diagram start time (h)",
                value = isolate({
                  ifelse(isTruthy(input$plot_starttime),
                         input$plot_starttime,
                         0)
                }),
                min = min_hour,
                max = max_hour,
                step = 1,
                type = "number"
              )
            ),
            field(
              labeled_numeric_input(
                ns("plot_endtime"),
                label = "Enter diagram end time (h)",
                value = isolate({
                  ifelse(isTruthy(input$plot_endtime),
                         input$plot_endtime,
                         24)
                }),
                min = min_hour,
                max = max_hour,
                step = 1,
                type = "number"
              )
            ),
            field(
              selectInput(
                ns("plot_aggr_interval"),
                label = "Select aggregate function",
                choices = aggr_func,
                selected = isolate({
                  ifelse(
                    isTruthy(input$plot_aggr_interval),
                    input$plot_aggr_interval,
                    aggr_func[1]
                  )
                }),
              )
            ))
            },
            field(
              selectInput(
                ns("plot_type"),
                label = "Select the diagram type",
                choices = plot_types,
                selected = isolate({
                  ifelse(isTruthy(input$plot_type),
                         input$plot_type,
                         plot_types[1])
                }),
              )
            )
          ), class = "my-10"))
        }
      })

      strat_counter <- reactiveVal(0)
      co_strat <- list()
      co_strat[[1]] <- eventSearchbarServer("strat1", database, is_realtime = T)
      co_strat[[2]] <- eventSearchbarServer("strat2", database, is_realtime = T)
      co_strat[[3]] <- eventSearchbarServer("strat3", database, is_realtime = T)
      co_strat[[4]] <- eventSearchbarServer("strat4", database, is_realtime = T)
      co_strat[[5]] <- eventSearchbarServer("strat5", database, is_realtime = T)

      ui_loaded_once <- reactiveVal(FALSE)

      output$plot_strat_form <- renderUI({
        req(input$cohort_picker, plot_data())
        isolate({
          segment(
            tagList(
              h3("Add a stratification"),
              tags$p(
                "Use stratification to compare outcomes for specific ICU stay populations.",
                tags$br(),
                tags$i(
                  "A stay assigned to a specific stratification n cannot be included in any n+1 other stratification."
                )
              ),
              fields(
                actionButton(
                  ns("add_stratification"),
                  label = icon("plus"),
                  class = "icon"
                ),
                actionButton(
                  ns("remove_stratification"),
                  label = icon("minus"),
                  class = "icon"
                )
              ),
              htmltools::tagAppendAttributes(
                segment(
                  "Stratification 1",
                  textInput(ns("strat1_label"), "Stratification name"),
                  eventSearchbarUI(ns("strat1")),
                  class = "red parameter-strat",
                  strat = "1"),
                style = if (ui_loaded_once()) "display:none;" else NULL
              ),
              htmltools::tagAppendAttributes(
                segment(
                  "Stratification 2",
                  textInput(ns("strat2_label"), "Stratification name"),
                  eventSearchbarUI(ns("strat2")),
                  class = "orange parameter-strat",
                  strat = "2"),
                style = if (ui_loaded_once()) "display:none;" else NULL
              ),
              htmltools::tagAppendAttributes(
                segment(
                  "Stratification 3",
                  textInput(ns("strat3_label"), "Stratification name"),
                  eventSearchbarUI(ns("strat3")),
                  class = "yellow parameter-strat",
                  strat = "3"),
                style = if (ui_loaded_once()) "display:none;" else NULL
              ),
              htmltools::tagAppendAttributes(
                segment(
                  "Stratification 4",
                  textInput(ns("strat4_label"), "Stratification name"),
                  eventSearchbarUI(ns("strat4")),
                  class = "olive parameter-strat",
                  strat = "4"),
                style = if (ui_loaded_once()) "display:none;" else NULL
              ),
              htmltools::tagAppendAttributes(
                segment(
                  "Stratification 5",
                  textInput(ns("strat5_label"), "Stratification name"),
                  eventSearchbarUI(ns("strat5")),
                  class = "green parameter-strat",
                  strat = "5"),
                style = if (ui_loaded_once()) "display:none;" else NULL
              ),
              htmltools::tagAppendAttributes(
                segment(
                  "Others",
                  textInput(ns("others_label"), "Stratification name"),
                  "Represent the remaining element that does not fit any stratification",
                  class = "blue parameter-strat others"),
                style = if (ui_loaded_once()) "display:none;" else NULL
              ),
              hidden(text_input(ns(
                "ui_loaded"
              ), value = "DOM Ready"))
            ),
            class = "my-10"
          )
        })
      })

      observeEvent(input$add_stratification, {
        if (strat_counter() < 5) {
          strat_counter(strat_counter() + 1)
        }
      })

      observeEvent(input$remove_stratification, {
        if (strat_counter() > 0) {
          strat_counter(strat_counter() - 1)
        }
      })

      last_cohort <- reactiveVal("")
      observe({
        req(input$cohort_picker)
        if(last_cohort() != input$cohort_picker){
          strat_counter(0)
          last_cohort(input$cohort_picker)
        }
      })

      observe({
        req(input$ui_loaded)
        input$ui_loaded
        runjs(
          paste0(
            "
        var strat_count = ",
            strat_counter(),
            ";
        const elements = document.querySelectorAll('.parameter-strat');

        elements.forEach(element => {
          const stratValue = parseInt(element.getAttribute('strat'), 10);
          if (stratValue <= strat_count) {
            element.style.display = 'block';
          } else{
            element.style.display = 'none';
          }
        });
          if (strat_count>0){
            document.querySelectorAll('.parameter-strat.others')[0].style.display = 'block';
          } else{
            document.querySelectorAll('.parameter-strat.others')[0].style.display = 'none';

          }"
          )
        )
        if(!isolate(ui_loaded_once())){
          ui_loaded_once(TRUE)
        }

      })

      output$plot_clean_form <- renderUI({
        req(input$cohort_picker, plot_data())
        min_value <-
          min(plot_data()$value, na.rm = T)
        max_value <-
          max(plot_data()$value, na.rm = T)
        stay_id_list <-
          (plot_data() %>% select(stay_id) %>% distinct())[["stay_id"]]
        htmltools::tagAppendAttributes(segment(field(
          checkbox_input(
            ns("plot_data_clean"),
            "Add a data cleaning",
            type = "toggle",
            is_marked = isolate({
              ifelse(isTruthy(input$plot_data_clean),
                     input$plot_data_clean,
                     FALSE)
            })
          )
          ,
          class = ""
        ), {
          if (isTruthy(input$plot_data_clean)) {
            tagList(tags$h3(tagList(
              icon(class = "grey broom"),
              tags$div("Data cleaning tools", class = "content")
            ), class = "ui header"),
            form(
              field(
                tags$label("Remove a list of stay_id (comma separated)"),
                text_input(
                  ns("plot_data_clean_stay_id"),
                  label = "",
                  value = isolate({
                    input$plot_data_clean_stay_id
                  })
                )
              ),
              fields(
                field(
                  labeled_numeric_input(
                    ns("plot_data_clean_min"),
                    label = "Min limit",
                    value = isolate({
                      ifelse(
                        isTruthy(input$plot_data_clean_min),
                        input$plot_data_clean_min,
                        max(0, min_value) #Almost no negative value are realistic
                      )
                    }),
                    min = min_value,
                    max = max_value,
                    type = "number"
                  )
                ),
                field(
                  labeled_numeric_input(
                    ns("plot_data_clean_max"),
                    label = "Max limit",
                    value = isolate({
                      ifelse(
                        isTruthy(input$plot_data_clean_max),
                        input$plot_data_clean_max,
                        min(max_value, 1000)  #Almost no value > 1000 are realistic
                      )
                    }),
                    min = min_value,
                    max = max_value,
                    type = "number"
                  )
                )
              )
            ))
          }
        }), class = "my-10")
      })
      output$plot_action <- renderUI({
        if (!isTruthy(input$cohort_picker)) {
          message_box(
            "No cohort is selected",
            "Please first select a cohort to access exploration tools",
            class = "info my-10"
          )
        } else{
          req(input$cohort_picker)
          if (isTruthy(plot_data())) {
            tags$div(
              tagList(
                htmltools::tagAppendAttributes(
                  button(
                    ns("plot_button"),
                    label = "Show Plot",
                    class = "teal"
                  ),
                  class =
                    "my-10"
                ),
                htmltools::tagAppendAttributes(
                  button(
                    ns("add_to_data_desc_button"),
                    label = "Add to Clinical Data Desc.",
                    icon = icon("table")
                  ),
                  class =
                    "my-10"
                ),
                downloadButton(
                  ns("export_data_csv"),
                  label = "Export Data to CSV",
                  class = "ui button basic green",
                  icon = icon("download icon")
                )
              ),
              class = ""
            )
          }
        }

      })

      unitname <- reactiveVal("None")
      strat_error_ui <- reactiveVal(NULL)

      apply_condition_object <- function(stay_to_filter,
                                         condition_object) {

        constraint_list <- condition_object$constraint_list
        table_count <- length(constraint_list)
        strat_data <- list()
        errors <- list()

        if (condition_object$condition_string != "") {
          for (condition_id in 1:table_count) {
            key <- paste0("condition_", condition_id)
            strat_data[[key]] <- tryCatch({
              dedicated_db_link <- connect_to_mimic()
              data <- get_constrained_table(
                dedicated_db_link,
                constraint_list[[as.character(condition_id)]]$linksto,
                constraint_list[[as.character(condition_id)]]$constraint,
                input$cohort_picker
              ) %>% collect()
              DBI::dbDisconnect(dedicated_db_link)
              data
            }, error = function(e) {
              tryCatch(DBI::dbDisconnect(dedicated_db_link), error = function(e2) NULL)
              errors[[key]] <<- conditionMessage(e)
              NULL
            })
          }
        }

        if (length(errors) > 0) {
          failed_details <- paste(
            sapply(names(errors), function(k) paste0("[", k, "] ", errors[[k]])),
            collapse = "<br>"
          )
          error_indices <- sapply(names(errors), function(k) as.integer(gsub("condition_", "", k)))
          return(list(
            status = "error",
            message = failed_details,
            error_indices = error_indices
          ))
        }

        # Check if any condition uses exclusion
        has_exclusion <- any(sapply(condition_object$constraint_list, function(c) isTRUE(c$constraint$is_exclusion)))
        universe <- NULL
        if (has_exclusion) {
          universe <- dplyr::tbl(database(), in_schema("public", "cohort")) %>%
            filter(cohort_id == !!input$cohort_picker) %>%
            select(subject_id, hadm_id, stay_id) %>%
            collect()
        }
        for (key in names(strat_data)) {
          condition_idx <- gsub("condition_", "", key)
          data <- strat_data[[key]]
          if (isTRUE(condition_object$constraint_list[[condition_idx]]$constraint$is_exclusion)) {
            # Pre-compute complement: stays in universe NOT matching this condition
            join_by <- if (!("stay_id" %in% names(data)) || is.null(data$stay_id[1])) {
              c("subject_id", "hadm_id")
            } else {
              c("subject_id", "hadm_id", "stay_id")
            }
            data <- anti_join(universe, data, by = join_by)
          }
          assign(key, data)
        }
        escaped_expression <-
          parsecondition(condition_object$condition_string)
        expression_to_eval <-
          gsub("[", "(", escaped_expression, fixed = T)
        expression_to_eval <-
          gsub("]", ")", expression_to_eval, fixed = T)

        strat_condition <-
          eval(parse(text = expression_to_eval))
        if (is.null(strat_condition)) {
          strat_condition <- isolate({
            dplyr::tbl(database(), in_schema("public", "cohort")) %>% filter(cohort_id == !!input$cohort_picker) %>% collect()
          })
        }

        icd_filtered_result <- process_icd_filter(database, condition_object, strat_condition)
        if (sum(is.na(strat_condition$stay_id)) == nrow(strat_condition) ||
            nchar(condition_object$icd_to_allow) > 0 ||
            nchar(condition_object$icd_to_deny) > 0) {
          toast(
            "",
            paste0(
              "Using a out-of-icu event to stratify, stay stratification may be innacurate."
            ),
            "yellow"
          )
        }
        intersect(icd_filtered_result$stay_id, stay_to_filter)
      }

      transformed_data <- reactive({
        req(plot_data())
        if (as.integer(input$plot_endtime) > as.integer(input$plot_starttime)) {
          progress <- Progress$new(session, min = 0, max = 5)
          if(input$plot_type != "Pie (categorical)"){
            progress$set(value = 1, message = 'Aggregating data')
            breaks <-
              seq(
                input$plot_starttime,
                input$plot_endtime,
                ifelse(
                  input$plot_timestep > input$plot_endtime - input$plot_starttime,
                  input$plot_endtime - input$plot_starttime,
                  input$plot_timestep
                )
              )
            data <- plot_data() %>%
              filter(
                hr >= as.integer(input$plot_starttime) &
                  hr <= as.integer(input$plot_endtime)
              ) %>%
              mutate(interval = cut(
                hr,
                breaks,
                include.lowest = TRUE,
                right = FALSE
              ))

            if (input$plot_data_clean) {
              stay_id_to_clean <-
                unlist(strsplit(input$plot_data_clean_stay_id, ",", fixed = TRUE))
              data <-
                data %>% filter(!(stay_id %in% stay_id_to_clean))
              min_accepted <-
                input$plot_data_clean_min
              max_accepted <-
                input$plot_data_clean_max
              data <-
                data %>% filter(value >= min_accepted &
                                  value <= max_accepted)
            }

            unitnames <- data[["unitname"]]
            u_unitname <-
              unique(unitnames[!(unitnames %in% list(NULL, "None"))])
            if (length(u_unitname) > 1) {
              toast(
               title = "",
               content = paste0(
                  "WARNING : Multiple unit name detected for this itemid ",
                  u_unitname
                ),
               color = "orange"
              )
              unitname(paste0(u_unitname, collapse = ","))
            } else{
              if(length(u_unitname)==1){
                unitname(u_unitname[[1]])
              } else{
                unitname("No Unit")
              }

            }

            # Aggregate data by hour for longitudinal trajectory so stay with more than one data per hour not biases the whole graph
            if (input$plot_type == "Longitudinal trajectory") {
              aggr_data <- data %>% group_by(interval, stay_id)
              #select(-interval) %>%
              #mutate(interval = hr) %>%
            } else{
              aggr_data <- data %>% group_by(interval, stay_id)
            }



            if (input$plot_aggr_interval == "mean") {
              aggr_data <- aggr_data %>%
                summarise(aggr = mean(value), .groups = 'drop')
            } else if (input$plot_aggr_interval == "min") {
              aggr_data <- aggr_data %>%
                summarise(aggr = min(value), .groups = 'drop')
            } else if (input$plot_aggr_interval == "max") {
              aggr_data <- aggr_data %>%
                summarise(aggr = max(value), .groups = 'drop')
            } else if (input$plot_aggr_interval == "median") {
              aggr_data <- aggr_data %>%
                summarise(aggr = median(value), .groups = 'drop')
            } else if (input$plot_aggr_interval == "sum") {
              aggr_data <- aggr_data %>%
                summarise(aggr = sum(value), .groups = 'drop')
            }
          } else{
            aggr_data <- plot_data() %>% mutate(interval = 1)
          }

          progress$set(value = 3, message = 'Stratificating data')

          if (strat_counter() > 0 ) {

            cohort <- dplyr::tbl(database(), in_schema("public", "cohort"))
            d_cohorts <- dplyr::tbl(database(), in_schema("public", "d_cohorts"))
            demographics <- dplyr::tbl(database(), in_schema("public", "demographics"))

            aggr_data_pre_strat <- aggr_data
            aggr_data <- d_cohorts %>%
              filter(cohort_id == !!input$cohort_picker) %>%
              inner_join(cohort, by = "cohort_id") %>%
              mutate(strat = "Whole Cohort") %>%
              collect() %>%
              {
                if (isolate(strat_counter()) > 0) {
                  stratified_stay_df <- .
                  strat_errors <- list()
                  # Clear error classes on all stratification searchbars
                  for (clear_i in 1:isolate(strat_counter())) {
                    runjs(paste0(
                      "$('#", ns(paste0("strat", clear_i)), "-filter-container').find('.ui.label.filter').each(function() {",
                      "   $(this).removeClass('event-error');",
                      "});"
                    ))
                  }
                  for (strat_i in 1:isolate(strat_counter())) {
                    selected_stay <- apply_condition_object((
                      stratified_stay_df %>% filter(strat == "Whole Cohort")
                    )$stay_id,co_strat[[strat_i]]())
                    if (is.list(selected_stay) && identical(selected_stay$status, "error")) {
                      # Highlight error conditions in this searchbar
                      if (!is.null(selected_stay$error_indices)) {
                        error_indices_js <- paste0("[", paste(selected_stay$error_indices, collapse = ","), "]")
                        runjs(paste0(
                          "let errIdx", strat_i, " = ", error_indices_js, ";",
                          "$('#", ns(paste0("strat", strat_i)), "-filter-container').find('.ui.label.filter').each(function(index) {",
                          "   if (errIdx", strat_i, ".includes(index + 1)) {",
                          "       $(this).addClass('event-error');",
                          "   }",
                          "});"
                        ))
                      } else {
                        # Whole request failed, mark all filters as error
                        runjs(paste0(
                          "$('#", ns(paste0("strat", strat_i)), "-filter-container').find('.ui.label.filter').addClass('event-error');"
                        ))
                      }
                      strat_errors[[length(strat_errors) + 1]] <- paste0(
                        "Stratification ", strat_i, ": ", selected_stay$message
                      )
                      next
                    }
                    stratified_stay_df <- stratified_stay_df %>% mutate(
                      strat = ifelse(
                        stay_id %in% selected_stay,ifelse(
                          input[[paste0("strat", strat_i, "_label")]] != "",
                          input[[paste0("strat", strat_i, "_label")]],
                          paste("Stratification", strat_i)
                        ) ,
                        strat
                      )
                    )
                  }
                  if (length(strat_errors) > 0) {
                    list(
                      status = "strat_error",
                      message = paste(strat_errors, collapse = "<br>")
                    )
                  } else {
                    stratified_stay_df %>% mutate(strat = ifelse(
                      strat == "Whole Cohort",
                      ifelse(
                        input$others_label != "",
                        input$others_label,
                        "Others"
                      ) ,
                      strat
                    ))
                  }
                } else{
                  .
                }

              }
            if (is.list(aggr_data) && identical(aggr_data$status, "strat_error")) {
              progress$close()
              strat_error_ui(message_box(
                "An error has occured",
                HTML(paste0(
                  "The following stratification(s) failed:<br><code>",
                  aggr_data$message,
                  "</code>"
                )),
                class = "negative my-10",
                closable = TRUE
              ))
              return(NULL)
            }
            strat_error_ui(NULL)
            aggr_data <- inner_join(aggr_data, aggr_data_pre_strat, by = "stay_id")
          } else{
            aggr_data <- aggr_data %>% mutate(strat = interval)
          }
          progress$close()
          aggr_data

        } else{
          toast("",
                paste0("Start time should be inferior to end time "),
                "red")
          NULL
        }


      })

      observeEvent(user_plot_click(), {
        if (nrow(user_plot_click()) == 1) {
          stay_id <- user_plot_click()$customdata
          icustays <-
            dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays"))
          selected_stay <-
            icustays %>%
            filter(stay_id == !!stay_id) %>%
            select(stay_id, hadm_id, subject_id) %>%
            collect()
          browseURL(
            paste0(
              input$url,
              "?subject_id=",
              selected_stay[["subject_id"]],
              "&hadm_id=",
              selected_stay[["hadm_id"]],
              "&stay_id=",
              selected_stay[["stay_id"]]
            )
          )
        }
      })

      user_plot <-
        eventReactive(input$plot_button, {
          req(strat_counter(),
              input$plot_type,
              transformed_data())
          labely <-
            paste0(as.character(
              distinct_events() %>% filter(itemid == !!input$parameter_picker) %>% select(label)
            ),
            " (",
            unitname(),
            ")")
          labelx <- "Time interval in hour"
          graph_color_palette <-
            RColorBrewer::brewer.pal(8, "Set2")
          if (strat_counter() > 0) {
                if(n_distinct(transformed_data()$strat)>1){
                  is_stratification_valid <- T
                } else{
                  is_stratification_valid <- F
                  toast(
                    "",
                    paste0(
                      "Your stratification return one empty set, your condition may target no stay or all of them"
                    ),
                    "yellow"
                  )
                }

          } else{
            is_stratification_valid <- F
          }
          suppressWarnings({
            print(transformed_data())
            if (input$plot_type == "Longitudinal trajectory") {
              labelx <- "Time in hour"
              strat_count <-
                length(levels(as.factor(transformed_data()$strat)))
              observation_count <-
                transformed_data() %>%
                count(interval, strat)
              median_aggr <-
                aggregate(aggr ~ interval + strat,
                          transformed_data(),
                          median,
                          na.rm = T)
              q1_aggr <-
                aggregate(
                  aggr ~ interval + strat,
                  transformed_data(),
                  quantile,
                  probs = c(0.25),
                  na.rm = T
                )
              q3_aggr <-
                aggregate(
                  aggr ~ interval + strat,
                  transformed_data(),
                  quantile,
                  probs = c(0.75),
                  na.rm = T
                )
              graph_data <-
                data.frame(
                  interval = median_aggr$interval,
                  strat = median_aggr$strat,
                  median_aggr = median_aggr$aggr,
                  ymin = q1_aggr$aggr,
                  ymax = q3_aggr$aggr
                )
              plot_ly(
                data = graph_data,
                x = ~ interval,
                y = ~ median_aggr,
                type = 'scatter',
                mode = 'lines',
                color = case_when(
                  is_stratification_valid ~ factor(graph_data$strat),
                  .default = graph_color_palette[1]
                ),
                name = case_when(
                  is_stratification_valid ~ as.character(graph_data$strat),
                  .default = labely
                ),
                colors = graph_color_palette[1:strat_count]
              ) %>%
                add_ribbons(
                  data = graph_data,
                  ymin = ~ ymin,
                  ymax = ~ ymax,
                  name = 'Q1/Q3',
                  fillcolor = case_when(
                    is_stratification_valid ~ adjustcolor(graph_color_palette[as.integer(factor(graph_data$strat))], alpha.f =
                                                            0.2),
                    .default = adjustcolor(graph_color_palette[1], alpha.f =
                                             0.2)
                  ),
                  line = list(width = 0)
                ) %>%
                add_trace(
                  data = observation_count,
                  y = ~ n,
                  x = ~ interval,
                  color = case_when(
                    is_stratification_valid ~ factor(observation_count$strat),
                    .default = "#FFFFFF"
                  ),
                  customdata = NULL,
                  hovertemplate = 'Observations : %{y} (%{text})',
                  text = ~ strat,
                  type = "bar",
                  yaxis = "y2",
                  opacity = 1,
                  width = 0.3,
                  showlegend = F,
                  offsetgroup = case_when(
                    is_stratification_valid ~ factor(observation_count$strat),
                    .default = factor(1)
                  ),
                  marker = list(color = "#AAA"),
                  name = "Total observations",
                  xaxis = 'x'
                ) %>%
                layout(
                  xaxis = list(title = labelx),
                  yaxis = list(title = labely, zeroline = F),
                  yaxis2 = list(
                    showline = FALSE,
                    side = "right",
                    overlaying = "y",
                    title = "Total observations",
                    range = list(0, max(observation_count$n) * 4),
                    showgrid = F
                  )
                )

            } else if (input$plot_type == "Boxplot") {
              plot <- stratified_boxplot(transformed_data(),
                                         labelx,
                                         labely,
                                         is_stratification_valid)
              event_register(plot, 'plotly_click')
              plot
            }
            else if (input$plot_type == "Violin") {
              plot <- stratified_violin_plot(transformed_data(),
                                             labelx,
                                             labely,
                                             is_stratification_valid)
              event_register(plot, 'plotly_click')
              plot
            } else if(input$plot_type == "Pie (categorical)"){
              plot <- stratified_pie(transformed_data(),
                             labely,
                             is_stratification_valid)
              output$pie_table <- stratified_table(transformed_data(),is_stratification_valid)
              event_register(plot, 'plotly_click')
              tagList(plot,DT::dataTableOutput(ns("pie_table")))
            }
          })
        })

      clinical_data_list <- reactiveVal(list())

      observeEvent(input$add_to_data_desc_button, {
        data_to_add <- transformed_data()
        label <- as.character(
          distinct_events() %>% filter(itemid == !!input$parameter_picker) %>% select(label)
        )
        data_to_add <-
          data_to_add %>% mutate(label = paste0(!!label, " ", interval)) %>%
          pivot_wider(names_from = "label", values_from = "aggr")
        clinical_data_list(append(clinical_data_list(), list(as_tibble(
          data_to_add
        ))))
      })



      observe({
        req(input$cohort_picker)
        clinicalDataDescServer("clinical_data_desc", database, clinical_data_list)
      })

      output$export_data_csv <- downloadHandler(
        filename = function() {
          paste0("custom_export_", input$parameter_picker, ".csv")
        },
        content = function(file) {
          write.csv(transformed_data(), file, row.names = FALSE)
        }
      )
      #Stay count, Sex Ratio, Age, Average LoS, Death %
      output$stratification_result <- renderUI({
        req(input$cohort_picker, user_plot())

        isolate({
          patients <-
            dplyr::tbl(database(), in_schema("mimiciv_hosp", "patients"))
          admissions <-
            dplyr::tbl(database(), in_schema("mimiciv_hosp", "admissions"))
          icustays <-
            dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays"))

          if (strat_counter() > 0) {
            if(n_distinct(transformed_data()$strat)>1){
              is_stratification_valid <- T
            } else{
              is_stratification_valid <- F
            }

          } else{
            is_stratification_valid <- F
          }
          if (is_stratification_valid) {
            strats <- levels(as.factor(transformed_data()$strat))
            strat_names <- strats

            # Compute per-strat stay_ids and table_stays
            stay_id_list <- lapply(strats, function(s) {
              unique((transformed_data() %>% filter(strat == s))$stay_id)
            })
            names(stay_id_list) <- strats

            table_stay_list <- lapply(strats, function(s) {
              copy_inline(database(),
                          as_tibble(list(stay_id = stay_id_list[[s]])),
                          types = c(stay_id = "bigint"))
            })
            names(table_stay_list) <- strats

            # Stay count
            stratification_summary <-
              list("Stay count" = lapply(strats, function(s) length(stay_id_list[[s]])))

            # Sex Ratio
            sex_ratios <- lapply(strats, function(s) {
              sex_count <-
                table_stay_list[[s]] %>% inner_join(icustays, by = "stay_id") %>% inner_join(patients, by =
                                                                                       "subject_id") %>%
                select(subject_id, gender) %>% distinct() %>% group_by(gender) %>% count() %>% collect() %>%
                pivot_wider(names_from = gender, values_from = n) %>% mutate_if(is.numeric, as.integer)
              sex_ratio_to_sex_count(sex_count)
            })
            stratification_summary <-
              append(stratification_summary,
                     list("Sex Ratio (M/F)" = sex_ratios))

            # Mean Age
            age_means <- lapply(strats, function(s) {
              (
                table_stay_list[[s]] %>% inner_join(icustays, by = "stay_id") %>% inner_join(patients, by =
                                                                                       "subject_id") %>%
                  mutate(age = anchor_age + DATE_PART('year', intime) - anchor_year) %>%
                  summarise(mean_age = mean(age)) %>% mutate(mean_age = round(mean_age, 2)) %>% collect()
              )$mean_age
            })
            stratification_summary <-
              append(stratification_summary, list("Mean Age" = age_means))

            # Mean Stay Length
            mean_stays <- lapply(strats, function(s) {
              (
                table_stay_list[[s]] %>% inner_join(icustays, by = "stay_id") %>%
                  mutate(stay_length = AGE(outtime, intime)) %>%
                  summarise(
                    mean_stay = sql('AVG(EXTRACT(EPOCH FROM "stay_length")/86400)')
                  ) %>% mutate(mean_stay = round(mean_stay, 2)) %>% collect()
              )$mean_stay
            })
            stratification_summary <-
              append(stratification_summary,
                     list("Mean Stay Length (in days)" = mean_stays))

            # % of death in Hospital
            d_percentages <- lapply(strats, function(s) {
              d_pct <- (
                table_stay_list[[s]] %>% inner_join(icustays, by = "stay_id") %>% inner_join(admissions, by =
                                                                                       c("subject_id", "hadm_id")) %>% inner_join(patients, by = "subject_id") %>%
                  mutate(death = as.integer(dod <= dischtime + days(1))) %>%
                  summarise(
                    d_percentage = sql('COALESCE(SUM("death"),0) / COUNT(*)::float * 100')
                  ) %>% mutate(d_percentage = round(d_percentage, 2)) %>% collect()
              )$d_percentage
              paste0(d_pct, "%")
            })
            stratification_summary <-
              append(stratification_summary,
                     list("% of death in Hospital" = d_percentages))

            # 30-day mortality
            d30_percentages <- lapply(strats, function(s) {
              d30_pct <- (
                table_stay_list[[s]] %>% inner_join(icustays, by = "stay_id") %>% inner_join(admissions, by =
                                                                                       c("subject_id", "hadm_id")) %>% inner_join(patients, by = "subject_id") %>%
                  mutate(death = as.integer(dod <= intime + days(30))) %>%
                  summarise(
                    d_percentage = sql('COALESCE(SUM("death"),0) / COUNT(*)::float * 100')
                  ) %>% mutate(d_percentage = round(d_percentage, 2)) %>% collect()
              )$d_percentage
              paste0(d30_pct, "%")
            })
            stratification_summary <-
              append(stratification_summary,
                     list("After ICU admission 30-day mortality" = d30_percentages))
          } else{
            strat_names <-
              c("Whole cohort (with data for this parameter in this time windows)")
            stay_ids <-
              unique((transformed_data()$stay_id))
            stratification_summary <-
              list("Stay count" = list(length(stay_ids)))

            table_stay <-
              copy_inline(database(),
                          as_tibble(list(stay_id = stay_ids)),
                          types = c(stay_id = "bigint"))

            sex_count <-
              table_stay %>% inner_join(icustays, by = "stay_id") %>% inner_join(patients, by =
                                                                                   "subject_id") %>%
              select(subject_id, gender) %>% distinct() %>% group_by(gender) %>% count() %>% collect() %>%
              pivot_wider(names_from = gender, values_from = n) %>% mutate_if(is.numeric, as.integer)
            sex_ratio <-
              sex_ratio_to_sex_count(sex_count)
            stratification_summary <-
              append(stratification_summary,
                     list("Sex Ratio (M/F)" = list(sex_ratio)))

            age_mean <-
              (
                table_stay %>% inner_join(icustays, by = "stay_id") %>% inner_join(patients, by =
                                                                                     "subject_id") %>%
                  mutate(age = anchor_age + DATE_PART('year', intime) - anchor_year) %>%
                  summarise(mean_age = mean(age)) %>% mutate(mean_age = round(mean_age, 2)) %>% collect()
              )$mean_age

            stratification_summary <-
              append(stratification_summary, list("Mean Age" = list(age_mean)))

            mean_stay <-
              (
                table_stay %>% inner_join(icustays, by = "stay_id") %>%
                  mutate(stay_length = AGE(outtime, intime)) %>%
                  summarise(
                    mean_stay = sql('AVG(EXTRACT(EPOCH FROM "stay_length")/86400)')
                  ) %>% mutate(mean_stay = round(mean_stay, 2)) %>% collect()
              )$mean_stay

            stratification_summary <-
              append(stratification_summary,
                     list("Mean Stay Length (in days)" = list(mean_stay)))

            d_percentage <-
              (
                table_stay %>% inner_join(icustays, by = "stay_id") %>% inner_join(admissions, by =
                                                                                     c("subject_id", "hadm_id")) %>% inner_join(patients, by = "subject_id") %>%
                  mutate(death = as.integer(dod <= dischtime + days(1))) %>%
                  summarise(
                    d_percentage = sql('COALESCE(SUM("death"),0) / COUNT(*)::float * 100')
                  ) %>% mutate(d_percentage = round(d_percentage, 2)) %>% collect()
              )$d_percentage

            stratification_summary <-
              append(stratification_summary,
                     list("% of death in Hospital" = list(paste0(
                       d_percentage, "%"
                     ))))

            d30_percentage <-
              (
                table_stay %>% inner_join(icustays, by = "stay_id") %>% inner_join(admissions, by =
                                                                                     c("subject_id", "hadm_id")) %>% inner_join(patients, by = "subject_id") %>%
                  mutate(death = as.integer(dod <= intime + days(30))) %>%
                  summarise(
                    d_percentage = sql('COALESCE(SUM("death"),0) / COUNT(*)::float * 100')
                  ) %>% mutate(d_percentage = round(d_percentage, 2)) %>% collect()
              )$d_percentage

            stratification_summary <-
              append(stratification_summary,
                     list("After ICU admission 30-day mortality" = list(paste0(
                       d30_percentage, "%"
                     ))))
          }


        })


        segment(
          tags$h3(tagList(
            icon(class = "grey search"),
            tags$div("Stratification Details", class = "content")
          ), class = "ui header"),
          tags$table(
            tags$thead(tags$tr(
              tags$th(),
              lapply(strat_names, function(s) tags$th(label(s, class = "")))
            )),
            tags$tbody(lapply(names(stratification_summary), function(name) {
              tags$tr(
                tags$th(name),
                lapply(seq_along(strat_names), function(i) {
                  tags$td(stratification_summary[[name]][[i]])
                })
              )
            })),
            class = "ui very basic collapsing celled table"
          ),
          class = "my-10"
        )
      })

      output$graph_render <- renderUI({
        if (!is.null(strat_error_ui())) return(strat_error_ui())
        req(input$cohort_picker, user_plot())
        if (input$plot_type %in% list("Longitudinal trajectory","Pie (categorical)")) {
          user_plot()
        } else{
          suppressWarnings(renderPlotly(user_plot()))
        }

      })

      user_plot_click <- reactive({
        req(user_plot())
        if (input$plot_type != "Longitudinal trajectory") {
          event_data("plotly_click", source = "user_plot")
        }
      })

      shinyjs::runjs(
        paste0(
          "$('body').on('change', '#",
          ns("plot_type"),
          "', function() {
                      let plot_type = $(this).find('input').val();
                      let plot_timestep_input = $('#",
          ns("plot_timestep"),
          "');
                    });
                     $('body').on('click', '#refresh-cohort-picker',function(){
                        Shiny.setInputValue('",
                          ns('update_cohort_picker'),
                          "', Date.now());
                    });
                    $('body').on('click', '#delete-cohort-picker',function(){
                      Shiny.setInputValue('",
                        ns('delete_cohort'),
                        "', Date.now());
                    });
                    $('body').on('click', '#edit-cohort-picker',function(){
                      Shiny.setInputValue('",
                        ns('edit_cohort'),
                        "', Date.now());
                    });
                    $('body').on('click', '#duplicate-cohort-picker',function(){
                      Shiny.setInputValue('",
                        ns('duplicate_cohort'),
                        "', Date.now());
                    });"
        )
      )

    }
  })
}
