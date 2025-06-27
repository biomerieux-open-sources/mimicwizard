cohortOutcomesExplorerServer <-
  function(id,
           database = NULL,
           cohort_id = NULL) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      co_strat <- list()
      co_strat[[1]] <- eventSearchbarServer("strat1", database, is_realtime = T)
      co_strat[[2]] <- eventSearchbarServer("strat2", database, is_realtime = T)
      co_strat[[3]] <- eventSearchbarServer("strat3", database, is_realtime = T)
      co_strat[[4]] <- eventSearchbarServer("strat4", database, is_realtime = T)
      co_strat[[5]] <- eventSearchbarServer("strat5", database, is_realtime = T)

      ui_loaded_once <- reactiveVal(FALSE)

      strat_counter <- reactiveVal(0)

      output$stratification_manager <- renderUI({
        if (!isTruthy(cohort_id())) {
          message_box(
            paste0("No cohort is selected", cohort_id()),
            "Please first select a cohort to access exploration tools",
            class = "info my-10"
          )
        } else{
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
                  class = "red outcome-strat",
                  strat = "1"),
                style = if (ui_loaded_once()) "display:none;" else NULL
                ),
                htmltools::tagAppendAttributes(
                  segment(
                    "Stratification 2",
                    textInput(ns("strat2_label"), "Stratification name"),
                    eventSearchbarUI(ns("strat2")),
                    class = "orange outcome-strat",
                    strat = "2"),
                  style = if (ui_loaded_once()) "display:none;" else NULL
                  ),
                  htmltools::tagAppendAttributes(
                    segment(
                      "Stratification 3",
                      textInput(ns("strat3_label"), "Stratification name"),
                      eventSearchbarUI(ns("strat3")),
                      class = "yellow outcome-strat",
                      strat = "3"),
                    style = if (ui_loaded_once()) "display:none;" else NULL
                    ),
                    htmltools::tagAppendAttributes(
                      segment(
                        "Stratification 4",
                        textInput(ns("strat4_label"), "Stratification name"),
                        eventSearchbarUI(ns("strat4")),
                        class = "olive outcome-strat",
                        strat = "4"),
                      style = if (ui_loaded_once()) "display:none;" else NULL
                      ),
                      htmltools::tagAppendAttributes(
                        segment(
                          "Stratification 5",
                          textInput(ns("strat5_label"), "Stratification name"),
                          eventSearchbarUI(ns("strat5")),
                          class = "green outcome-strat",
                          strat = "5"),
                        style = if (ui_loaded_once()) "display:none;" else NULL
                        ),
                        htmltools::tagAppendAttributes(
                          segment(
                            "Others",
                            textInput(ns("others_label"), "Stratification name"),
                            "Represent the remaining element that does not fit any stratification",
                            class = "blue outcome-strat others"),
                          style = if (ui_loaded_once()) "display:none;" else NULL
                          ),
                          hidden(text_input(ns(
                            "ui_loaded"
                          ), value = "DOM Ready")),
                          actionButton(ns("execute_stratification"), label =
                                         "View Stratification")
                        ),
                        class = "my-10"
                      )
          })
      }

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
        req(cohort_id())
        if(last_cohort() != cohort_id()){
          strat_counter(0)
          last_cohort(cohort_id())
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
        const elements = document.querySelectorAll('.outcome-strat');

        elements.forEach(element => {
          const stratValue = parseInt(element.getAttribute('strat'), 10);
          if (stratValue <= strat_count) {
            element.style.display = 'block';
          } else{
            element.style.display = 'none';
          }
        });"
          )
        )
        if (strat_counter() > 0) {
          runjs(
            "document.querySelectorAll('.outcome-strat.others')[0].style.display = 'block';"
          )

        } else{
          runjs(
            "document.querySelectorAll('.outcome-strat.others')[0].style.display = 'none';"
          )

        }
        ui_loaded_once(TRUE)
      })

      contains_number <- function(x) {
        any(grepl("\\d", x))
      }

      apply_condition_object <- function(stay_to_filter,
                                         condition_object) {
        strat_data <-
          request_constrained(
            condition_object,
            is_synchronous = T,
            cohort_filter = input$cohort_picker
          )
        for (key in names(strat_data)) {
          assign(key, strat_data[[key]])
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


      output$outcomes_result <- renderUI({
        if (isTruthy(cohort_id())) {
          cohort <- dplyr::tbl(database(), in_schema("public", "cohort"))
          d_cohorts <- dplyr::tbl(database(), in_schema("public", "d_cohorts"))
          demographics <- dplyr::tbl(database(), in_schema("public", "demographics"))

          stratified_data <- d_cohorts %>%
            filter(cohort_id == !!cohort_id()) %>%
            inner_join(cohort, by = "cohort_id") %>%
            inner_join(demographics,
                       by = c("subject_id", "hadm_id", "stay_id")) %>%
            mutate(strat = "Whole Cohort") %>%
            collect() %>%
            {
              if (isolate(strat_counter()) > 0) {
                stratified_stay_df <- .
                for (strat_i in 1:isolate(strat_counter())) {
                  selected_stay <- apply_condition_object((
                    stratified_stay_df %>% filter(strat == "Whole Cohort")
                  )$stay_id,co_strat[[strat_i]]())
                  stratified_stay_df <- stratified_stay_df %>% mutate(
                    strat = ifelse(
                      stay_id %in% selected_stay,ifelse(
                          !!input[[paste0("strat", strat_i, "_label")]] != "",
                          !!input[[paste0("strat", strat_i, "_label")]],
                          paste("Stratification", strat_i)
                        ) ,
                     strat
                    )
                  )
                }
                stratified_stay_df %>% mutate(strat = ifelse(
                  strat == "Whole Cohort",
                  ifelse(
                    !!input$others_label != "",
                    !!input$others_label,
                    "Others"
                  ) ,
                  strat
                ))
              } else{
                .
              }

            }
          wider_stratified <- stratified_data %>%
            transmute(strat,label, value,valueuom) %>%
            group_by(strat,label,valueuom) %>%
            mutate(rn = row_number()) %>%
            pivot_wider(names_from = c("label","valueuom"),names_glue= "{label} ({valueuom})", values_from = "value") %>%
            select(-rn)

          cols_to_convert <- names(wider_stratified)[sapply(wider_stratified, contains_number)]

          cols_to_convert <- setdiff(cols_to_convert, "strat")

          wider_stratified <- wider_stratified %>%
            mutate_at(cols_to_convert, as.numeric)

          names(wider_stratified) <- gsub(" \\(\\)", "", names(wider_stratified))

          strat_number <- isolate(strat_counter())

          diagnoses_icd <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "diagnoses_icd"))
          d_icd_diagnoses <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "d_icd_diagnoses"))
          icd_data <- stratified_data %>% select(subject_id,hadm_id,strat) %>% distinct() %>% {
            inner_join(x=diagnoses_icd,y=.,by=c("subject_id","hadm_id"),copy=TRUE)
          } %>%
            filter(seq_num <= 5) %>% inner_join(d_icd_diagnoses,by=c("icd_version","icd_code")) %>%
          count(icd_code,icd_version,long_title,strat,sort = TRUE) %>% collect()
          segment_color <- c("red","orange","yellow","olive","green","blue")

          withSpinner(tagList(render_gt({
            tbl_summary(
              wider_stratified,
              include = c(
                "age (y.o.)",
                "gender",
                "weight (kg)",
                "height (cm)",
                "BMI (kg/m2)",
                "Charlson Score at hospital admission",
                "Simplified Acute Physiology Score (SAPSII)",
                "Sequential Organ Failure Assessment score (SOFA)",
                "Length of ICU Stay (LIS) (hr)",
                "Length of Hospital Stay (LHS) (hr)",
                "Death in ICU",
                "Death in Hospital",
                "30-day after ICU admission death",
                "1-year after ICU admission death"
              ),
              type = c("Sequential Organ Failure Assessment score (SOFA)") ~ "continuous",
              {
                if (strat_number > 0) {
                  by = "strat"
                }
              },
              # split table by group
              missing = "no" # don't list missing data separately
            ) %>%
              add_n() %>%
              {
                if (strat_number > 0) {
                    add_p(.) %>%
                    add_q()
                } else{
                  .
                }
              } %>%
              bold_labels() %>%
              as_gt() %>%
              tab_row_group("Baseline characteristics",
                            rows=if(length(as.factor(
                              unique(wider_stratified$gender)
                            )) == 2) 1:8 else 1:7) %>%
              tab_row_group("Parameters at ICU admission",
                            rows=if(length(as.factor(
                              unique(wider_stratified$gender)
                            )) == 2) 9:10 else 8:9) %>%
              tab_row_group("Outcomes", rows=if(length(as.factor(
                unique(wider_stratified$gender)
              )) == 2) 11:(length(.)-1) else 10:(length(.)-2)) %>%
              row_group_order(
                groups = c(
                  "Baseline characteristics",
                  "Parameters at ICU admission",
                  "Outcomes"
                )
              )
          }), div(tagList(
            lapply(c(
              "strat",
              "age (y.o.)",
              "gender",
              "weight (kg)",
              "height (cm)",
              "BMI (kg/m2)",
              "Charlson Score at hospital admission",
              "Simplified Acute Physiology Score (SAPSII)",
              "Sequential Organ Failure Assessment score (SOFA)",
              "Length of ICU Stay (LIS) (hr)",
              "Length of Hospital Stay (LHS) (hr)",
              "Death in ICU",
              "Death in Hospital",
              "30-day after ICU admission death",
              "1-year after ICU admission death"
            ), function(name) {
              if(length(as.factor(
                unique(wider_stratified$strat)
              )) > 1){
                args <- list(formula=wider_stratified[[name]] ~ wider_stratified$strat, main = name)
              }else{
                args <- list(x=wider_stratified[[name]], main = name)
              }
              div(
                renderPlot(
                  do.call(DescTools::Desc,args),
                  height = 300
                ),
                class = "four wide column",
                style = "height:300px"
              )
            })
          ), class = "ui grid"),div(
            if(strat_counter()>0){
              tagList(
              lapply(1:isolate(strat_counter()), function(strat_i) {
                strat_name <- ifelse(
                  !!input[[paste0("strat", strat_i, "_label")]] != "",
                  !!input[[paste0("strat", strat_i, "_label")]],
                  paste("Stratification", strat_i)
                )
                div(segment(tags$h5(paste0("Most frequents ICD code for ",strat_name)),tags$table(
                  class = "ui very basic collapsing celled table",
                  tags$tr(tags$th("ICD Code"),tags$th("Count")),
                  apply(icd_data %>% filter(strat == !!strat_name) %>% head(5), 1, function(icd_row) {
                    tags$tr(
                      tags$td(paste0(icd_row["long_title"], " ", trimws(icd_row["icd_code"]), "v", trimws(icd_row["icd_version"]))),
                      tags$td(icd_row["n"])
                    )
                  })
                )
                ,class=segment_color[strat_i],style="height:350px;"),class = "four wide column")
              }),
              div(segment(tags$h5(paste0("Most frequents ICD code for ",ifelse(
                !!input$others_label != "",
                !!input$others_label,
                "Others"
              ))),tags$table(
                class = "ui very basic collapsing celled table",
                tags$tr(tags$th("ICD Code"),tags$th("Count")),
                apply(icd_data %>% filter(strat == ifelse(
                  !!input$others_label != "",
                  !!input$others_label,
                  "Others"
                )) %>% head(5), 1, function(icd_row) {
                  tags$tr(
                    tags$td(paste0(icd_row["long_title"], " ", trimws(icd_row["icd_code"]), "v", trimws(icd_row["icd_version"]))),
                    tags$td(icd_row["n"])
                  )
                })
              )
              ,class=segment_color[6],style="height:350px;"),class = "four wide column"))
            } else{

              div(segment(tags$h5("Most frequents ICD code"),tags$table(
                class = "ui very basic collapsing celled table",
                tags$tr(tags$th("ICD Code"),tags$th("Count")),
                apply(icd_data %>% filter(strat == "Whole Cohort") %>% head(5), 1, function(icd_row) {
                  tags$tr(
                    tags$td(paste0(icd_row["long_title"], " ", trimws(icd_row["icd_code"]), "v", trimws(icd_row["icd_version"]))),
                    tags$td(icd_row["n"])
                  )
                })
              )
              ,class="blue",style="height:350px;")
              ,class = "four wide column")
            }
            , class = "ui grid"),div(tags$i("ICD Code are taken from the 5 main ICD for each patient (seq_num <= 5)"),class="my-10")))
          #cols_add(plot=list(,renderPlot(DescTools::Desc(cohort_data$gender)),"","","weight","height","BMI","Charlson Score at hospital admission","Simplified Acute Physiology Score (SAPSII)","Sequential Organ Failure Assessment score (SOFA)","Length of ICU Stay (LIS)","Length of Hospital Stay (LHS)"))

        }

      }) %>% bindEvent({
        input$execute_stratification
        cohort_id()
      }, ignoreInit = FALSE, ignoreNULL = TRUE)
      })
      }
