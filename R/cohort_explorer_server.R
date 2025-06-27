cohortExplorerServer <- function(id,
                                 database = NULL,
                                 selected_profile = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    if (!is.null(database)) {
      selected_profile <- session$userData$selected_profile

      condition_object <-
        eventSearchbarServer(
          "event_searchbar",
          database,
          search_label = "Select the parameter(s) you want to use for stratification",
          is_realtime = T
        )

      cohortOutcomesExplorerServer("cohort_outcomes_explorer",database,isolate(reactive(input$cohort_picker)))

      cohort_restriction <- reactive({
        selected_profile
        as.numeric(unlist(selected_profile()$user_cohorts))
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
                icon("pointer-scale grey trash", id = "delete-cohort-picker")
                ),style="width:30px;display:inline-flex;"
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
      output$field_picker <- renderUI({
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


      output$force_cast <- renderUI({
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
                paste0("Some value that you're trying to observe is non numeric or null (",round(sum(is.na(as.numeric(plot_data()$value)))/nrow(plot_data()),2)*100,"%). Some function may return error/empty diagram. Ignore this message if this is expected, otherwise, consider using numeric cast"),
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


      output$plot_strat_form <- renderUI({
        req(input$cohort_picker, plot_data())
        htmltools::tagAppendAttributes(segment(form(field(
          checkbox_input(
            ns("plot_is_stratified"),
            "Add a stratification",
            type = "toggle",
            is_marked = isolate({
              ifelse(isTruthy(input$plot_is_stratified),
                     input$plot_is_stratified,
                     FALSE)
            })
          )
        ), {
          if (isTruthy(input$plot_is_stratified)) {
            tagList(
              tags$h3(tagList(
                icon(class = "grey id card outline"),
                tags$div("Demographic Stratification", class = "content")
              ), class = "ui header"),
              fields(
                field(
                  selectInput(
                    ns("plot_strat_dem_param"),
                    label = "Select a static/demographic parameter",
                    choices = list(
                      "",
                      "Gender",
                      "Age",
                      "HADM Length",
                      "In hospital death",
                      "< 1 year after HADM death"
                    ),
                    selected = isolate({
                      ifelse(
                        isTruthy(input$plot_strat_dem_param),
                        input$plot_strat_dem_param,
                        ""
                      )
                    }),
                  )
                ),
                field(
                  selectInput(
                    ns("plot_strat_dem_cond"),
                    label = "Select a condition",
                    choices = list("==", "<", ">", "EXIST"),
                    selected = isolate({
                      ifelse(
                        isTruthy(input$plot_strat_dem_cond),
                        input$plot_strat_dem_cond,
                        "=="
                      )
                    }),
                  )
                ),
                field(
                  labeled_numeric_input(
                    ns("plot_strat_dem_cond_value"),
                    label = "Select a value for your condition (optionnal)",
                    type = "number",
                    value = isolate({
                      ifelse(
                        isTruthy(input$plot_strat_dem_cond_value),
                        input$plot_strat_dem_cond_value,
                        ""
                      )
                    })
                  )
                )
              ),
              tags$div("OR", class = "ui horizontal divider"),
              tags$h3(tagList(
                icon(class = "grey file medical alternate"),
                tags$div(
                  "Event based restriction",
                  div(
                    "(event based restriction will be applied only if demographic stratification is not set)",
                    class = "sub header"
                  ),
                  class = "content"
                )
              ), class = "ui header"),
              eventSearchbarUI(ns("event_searchbar"))
            )
          }
        })), class = "my-10")
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
              aggr_data <- data %>% select(-interval) %>%
                mutate(interval = hr) %>% group_by(interval, stay_id)
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
          if (input$plot_is_stratified &&
              (isTruthy(condition_object()) |
               input$plot_strat_dem_param != "")) {
            if (input$plot_strat_dem_param == "") {
              user_condition_object <- condition_object()
              strat_data <-
                request_constrained(
                  user_condition_object,
                  is_synchronous = T,
                  cohort_filter = input$cohort_picker
                )
              for (key in names(strat_data)) {
                assign(key, strat_data[[key]])
              }
              escaped_expression <-
                parsecondition(user_condition_object$condition_string)
              expression_to_eval <-
                gsub("[", "(", escaped_expression, fixed = T)
              expression_to_eval <-
                gsub("]", ")", expression_to_eval, fixed = T)

              strat_condition <-
                eval(parse(text = expression_to_eval))
              progress$set(value = 4, message = 'Processing ICD filter')
              if(is.null(strat_condition)){
                strat_condition <- isolate({dplyr::tbl(database(), in_schema("public", "cohort")) %>% filter(cohort_id == !!input$cohort_picker) %>% collect()})
              }

              icd_filtered_result <- process_icd_filter(database,user_condition_object,strat_condition)
              if (sum(is.na(strat_condition$stay_id)) == nrow(strat_condition) || nchar(user_condition_object$icd_to_allow) > 0 || nchar(user_condition_object$icd_to_deny) > 0) {
                toast(
                  "",
                  paste0(
                    "Using a out-of-icu event to stratify, stay stratification may be innacurate."
                  ),
                  "yellow"
                )
              }
              strat_stay <-
                icd_filtered_result$stay_id
              aggr_data <-
                aggr_data %>% mutate(strat = case_when((stay_id %in% strat_stay) ~ "Event condition",
                                                       .default = "Others"
                ))


            }
            else{
              if (isTruthy(condition_object())) {
                shiny.semantic::toast(
                  message = paste0(
                    "<b>Demographic and event based stratification are filled</b><br>
                                Only demographic stratification will be applied"
                  ),
                  class = "warning",
                  duration = 10
                )
              }
              patients <-
                dplyr::tbl(database(),
                           in_schema("mimiciv_hosp", "patients"))
              admissions <-
                dplyr::tbl(database(),
                           in_schema("mimiciv_hosp", "admissions"))
              icustays <-
                dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays"))
              if (input$plot_strat_dem_param == "Gender") {
                gender_list <-
                  patients %>% inner_join(icustays, by = "subject_id") %>% select(stay_id, gender) %>% collect()
                aggr_data <-
                  aggr_data %>% inner_join(gender_list, by = "stay_id") %>% rename(strat =
                                                                                     gender)

              } else if (input$plot_strat_dem_param == "Age") {
                if (!is.numeric(input$plot_strat_dem_cond_value)) {
                  toast("",
                        paste0("Expecting an Age value to stratify "),
                        "red")
                  progress$close()
                  return(NULL)
                }

                age_list <- patients %>%
                  inner_join(icustays, by = "subject_id") %>%
                  inner_join(admissions, by = "hadm_id") %>%
                  mutate(age = anchor_age + DATE_PART('year', admittime) - anchor_year) %>%
                  select(stay_id, age) %>% collect()

                mutate_request <-
                  paste0(
                    " case_when(age ",
                    input$plot_strat_dem_cond,
                    " ",
                    input$plot_strat_dem_cond_value,
                    " ~ '",
                    input$plot_strat_dem_param,
                    input$plot_strat_dem_cond,
                    input$plot_strat_dem_cond_value,
                    "', .default = 'others')"
                  )
                aggr_data <-
                  aggr_data %>% inner_join(age_list, by = "stay_id") %>% mutate(
                    strat := !!rlang::parse_quo(mutate_request, env = rlang::caller_env())
                  )

              } else if (input$plot_strat_dem_param == "HADM Length") {
                if (!is.numeric(input$plot_strat_dem_cond_value)) {
                  toast("",
                        paste0("Expecting an HADM Length value to stratify "),
                        "red")
                  progress$close()
                  return(NULL)
                }

                stay_length_list <- patients %>%
                  inner_join(icustays, by = "subject_id") %>%
                  inner_join(admissions, by = "hadm_id") %>%
                  mutate(hadm_length = AGE(dischtime, admittime)) %>%
                  select(stay_id, hadm_length) %>% collect()

                mutate_request <-
                  paste0(
                    " case_when(hadm_length ",
                    input$plot_strat_dem_cond,
                    " ",
                    input$plot_strat_dem_cond_value,
                    " ~ '",
                    input$plot_strat_dem_param,
                    input$plot_strat_dem_cond,
                    input$plot_strat_dem_cond_value,
                    "', .default = 'others')"
                  )
                aggr_data <-
                  aggr_data %>% inner_join(stay_length_list, by = "stay_id") %>% mutate(
                    strat := !!rlang::parse_quo(mutate_request, env = rlang::caller_env())
                  )

              } else if (input$plot_strat_dem_param == "In hospital death") {
                in_hosp_death_list <- patients %>%
                  inner_join(icustays, by = "subject_id") %>%
                  inner_join(admissions, by = "hadm_id") %>%
                  mutate(
                    in_hosp_death = case_when(
                      dischtime + days(1) >= dod ~ "In hospital death",
                      .default = "Others"
                    )
                  ) %>%
                  select(stay_id, in_hosp_death) %>% collect()
                aggr_data <-
                  aggr_data %>% inner_join(in_hosp_death_list, by = "stay_id") %>% rename(strat =
                                                                                            in_hosp_death)
              } else if (input$plot_strat_dem_param == "< 1 year after HADM death") {
                one_y_after_hosp_death_list <- patients %>%
                  inner_join(icustays, by = "subject_id") %>%
                  inner_join(admissions, by = "hadm_id") %>%
                  mutate(
                    one_y_after_hosp_death = case_when(
                      dischtime + years(1) >= dod ~ "< 1 year after HADM death",
                      .default = "Others"
                    )
                  ) %>%
                  select(stay_id, one_y_after_hosp_death) %>% collect()
                aggr_data <-
                  aggr_data %>% inner_join(one_y_after_hosp_death_list, by = "stay_id") %>% rename(strat =
                                                                                                     one_y_after_hosp_death)
              }
            }
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
          req(input$cohort_picker,
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
          if (input$plot_is_stratified &&
              (isTruthy(condition_object()) |
               input$plot_strat_dem_param != "")) {
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

      observeEvent(input$reset_data_desc_button, {
        clinical_data_list(list())
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

          if (input$plot_is_stratified &&
              (isTruthy(condition_object()) |
               input$plot_strat_dem_param != "")) {
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
            stratA <- strats[1]
            stratB <- strats[2]

            stay_id_A <-
              unique((transformed_data() %>% filter(strat == stratA))$stay_id)
            stay_id_B <-
              unique((transformed_data() %>% filter(strat == stratB))$stay_id)
            stratification_summary <-
              list("Stay count" = list(length(stay_id_A), length(stay_id_B)))

            table_stay_A <-
              copy_inline(database(),
                          as_tibble(list(stay_id = stay_id_A)),
                          types = c(stay_id = "bigint"))
            table_stay_B <-
              copy_inline(database(),
                          as_tibble(list(stay_id = stay_id_B)),
                          types = c(stay_id = "bigint"))


            sex_count_A <-
              table_stay_A %>% inner_join(icustays, by = "stay_id") %>% inner_join(patients, by =
                                                                                     "subject_id") %>%
              select(subject_id, gender) %>% distinct() %>% group_by(gender) %>% count() %>% collect() %>%
              pivot_wider(names_from = gender, values_from = n) %>% mutate_if(is.numeric, as.integer)
            sex_count_B <-
              table_stay_B %>% inner_join(icustays, by = "stay_id") %>% inner_join(patients, by =
                                                                                     "subject_id") %>%
              select(subject_id, gender) %>% distinct() %>% group_by(gender) %>% count() %>% collect() %>%
              pivot_wider(names_from = gender, values_from = n) %>% mutate_if(is.numeric, as.integer)
            sex_ratio_A <-
              sex_ratio_to_sex_count(sex_count_A)
            sex_ratio_B <-
              sex_ratio_to_sex_count(sex_count_B)
            stratification_summary <-
              append(stratification_summary,
                     list("Sex Ratio (M/F)" = list(sex_ratio_A, sex_ratio_B)))

            age_mean_A <-
              (
                table_stay_A %>% inner_join(icustays, by = "stay_id") %>% inner_join(patients, by =
                                                                                       "subject_id") %>%
                  mutate(age = anchor_age + DATE_PART('year', intime) - anchor_year) %>%
                  summarise(mean_age = mean(age)) %>% mutate(mean_age = round(mean_age, 2)) %>% collect()
              )$mean_age
            age_mean_B <-
              (
                table_stay_B %>% inner_join(icustays, by = "stay_id") %>% inner_join(patients, by =
                                                                                       "subject_id") %>%
                  mutate(age = anchor_age + DATE_PART('year', intime) - anchor_year) %>%
                  summarise(mean_age = mean(age)) %>% mutate(mean_age = round(mean_age, 2)) %>% collect()
              )$mean_age

            stratification_summary <-
              append(stratification_summary, list("Mean Age" = list(age_mean_A, age_mean_B)))

            mean_stay_A <-
              (
                table_stay_A %>% inner_join(icustays, by = "stay_id") %>%
                  mutate(stay_length = AGE(outtime, intime)) %>%
                  summarise(
                    mean_stay = sql('AVG(EXTRACT(EPOCH FROM "stay_length")/86400)')
                  ) %>% mutate(mean_stay = round(mean_stay, 2)) %>% collect()
              )$mean_stay
            mean_stay_B <-
              (
                table_stay_B %>% inner_join(icustays, by = "stay_id") %>%
                  mutate(stay_length = AGE(outtime, intime)) %>%
                  summarise(
                    mean_stay = sql('AVG(EXTRACT(EPOCH FROM "stay_length")/86400)')
                  ) %>% mutate(mean_stay = round(mean_stay, 2)) %>% collect()
              )$mean_stay

            stratification_summary <-
              append(stratification_summary,
                     list(
                       "Mean Stay Length (in days)" = list(mean_stay_A, mean_stay_B)
                     ))

            d_percentage_A <-
              (
                table_stay_A %>% inner_join(icustays, by = "stay_id") %>% inner_join(admissions, by =
                                                                                       c("subject_id", "hadm_id")) %>% inner_join(patients, by = "subject_id") %>%
                  mutate(death = as.integer(dod <= dischtime + days(1))) %>%
                  summarise(
                    d_percentage = sql('COALESCE(SUM("death"),0) / COUNT(*)::float * 100')
                  ) %>% mutate(d_percentage = round(d_percentage, 2)) %>% collect()
              )$d_percentage
            d_percentage_B <-
              (
                table_stay_B %>% inner_join(icustays, by = "stay_id") %>% inner_join(admissions, by =
                                                                                       c("subject_id", "hadm_id")) %>% inner_join(patients, by = "subject_id") %>%
                  mutate(death = as.integer(dod <= dischtime + days(1))) %>%
                  summarise(
                    d_percentage = sql('COALESCE(SUM("death"),0) / COUNT(*)::float * 100')
                  ) %>% mutate(d_percentage = round(d_percentage, 2)) %>% collect()
              )$d_percentage
            stratification_summary <-
              append(stratification_summary,
                     list("% of death in Hospital" = list(
                       paste0(d_percentage_A, "%"),
                       paste0(d_percentage_B, "%")
                     )))

            d30_percentage_A <-
              (
                table_stay_A %>% inner_join(icustays, by = "stay_id") %>% inner_join(admissions, by =
                                                                                       c("subject_id", "hadm_id")) %>% inner_join(patients, by = "subject_id") %>%
                  mutate(death = as.integer(dod <= intime + days(30))) %>%
                  summarise(
                    d_percentage = sql('COALESCE(SUM("death"),0) / COUNT(*)::float * 100')
                  ) %>% mutate(d_percentage = round(d_percentage, 2)) %>% collect()
              )$d_percentage
            d30_percentage_B <-
              (
                table_stay_B %>% inner_join(icustays, by = "stay_id") %>% inner_join(admissions, by =
                                                                                       c("subject_id", "hadm_id")) %>% inner_join(patients, by = "subject_id") %>%
                  mutate(death = as.integer(dod <= intime + days(30))) %>%
                  summarise(
                    d_percentage = sql('COALESCE(SUM("death"),0) / COUNT(*)::float * 100')
                  ) %>% mutate(d_percentage = round(d_percentage, 2)) %>% collect()
              )$d_percentage
            stratification_summary <-
              append(stratification_summary,
                     list("After ICU admission 30-day mortality" = list(
                       paste0(d30_percentage_A, "%"),
                       paste0(d30_percentage_B, "%")
                     )))
          } else{
            stratA <-
              "Whole cohort (with data for this parameter in this time windows)"
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
          tags$table(tags$thead(tags$tr(
            tags$th(), tags$th(label(stratA, class = "")), {
              if (is_stratification_valid)
                tags$th(label(stratB, class = ""))
            }

          ), ), tags$tbody(lapply(names(stratification_summary), function(name) {
            tags$tr(tags$th(name),
                    tags$td(stratification_summary[[name]][1]),
                    {
                      if (is_stratification_valid)
                        tags$td(stratification_summary[[name]][2])
                    })
          })), class = "ui very basic collapsing celled table"),
          class = "my-10"
        )
      })

      output$graph_render <- renderUI({
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
                      console.log(plot_type)
                      if(plot_type == 'Longitudinal trajectory'){
                        console.log(plot_timestep_input.attr('disabled','disabled'))
                        plot_timestep_input.attr('disabled','disabled');
                        plot_timestep_input.val('1');
                        plot_timestep_input.parent().attr('data-tooltip','Longitudinal trajectory plot type need a timestep equal to 1');
                        plot_timestep_input.parent().attr('data-position','right center');
                      }else{
                        plot_timestep_input.removeAttr('disabled');
                      plot_timestep_input.parent().removeAttr('data-tooltip');
                      }
                    });
                     $('body').on('click', '#refresh-cohort-picker',function(){
                    Shiny.setInputValue('update_profile', Date.now());
                        Shiny.setInputValue('",
                          ns('update_cohort_picker'),
                          "', Date.now());
                    });
                    $('body').on('click', '#delete-cohort-picker',function(){
                      Shiny.setInputValue('",
                        ns('delete_cohort'),
                        "', Date.now());
                    });"
        )
      )

    }
  })
}
