patientExplorerServer <-
  function(id,
           database = NULL,
           cohort_id = -1) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      if (!is.null(database)) {
        selected_profile <- session$userData$selected_profile

        #-------------------------------------------------------------------------------------------------------#
        ####
        ####  Patient FAST Selection - All fields
        ####
        #-------------------------------------------------------------------------------------------------------#

        default_itemid <- reactive({
          if (!is.null(selected_profile)) {
            selected_profile()$user_itemids
          } else{
            list()
          }
        })


        uid_list <- reactive({
          if (isTruthy(cohort_id) && cohort_id == -1) {
            ifelse(!dir.exists(paste0(
              CONFIG$CACHE_DIR, "cache/patient_events/"
            )),
            dir.create(
              paste0(CONFIG$CACHE_DIR, "cache/patient_events/"),
              recursive = TRUE
            ),
            FALSE)
            if (file.exists(paste0(
              CONFIG$CACHE_DIR,
              "cache/patient_events/uid_list.Rda"
            ))) {
              uid_list <-
                as_tibble(readRDS(
                  paste0(
                    CONFIG$CACHE_DIR,
                    "cache/patient_events/uid_list.Rda"
                  )
                ))
            } else{
              uid_list <-
                dplyr::tbl(database(), in_schema("public", "uid_list")) %>% collect()
              saveRDS(
                uid_list,
                file = paste0(
                  CONFIG$CACHE_DIR,
                  "cache/patient_events/uid_list.Rda"
                )
              )
            }

          } else if (isTruthy(cohort_id) &&
                     cohort_id != -1) {
            uid_list <-
              dplyr::tbl(database(), in_schema("public", "cohort")) %>%
              filter(cohort_id == !!cohort_id) %>%
              collect() %>%
              pivot_longer(
                c("subject_id", "hadm_id", "stay_id"),
                names_to = "type",
                values_to = "uid"
              ) %>%
              distinct() %>%
              drop_na()

          } else{
            uid_list <- NULL
          }
          uid_list
        })

        search_api_uid <- function(data, q, database) {
          has_matching <- function(field) {
            startsWith(as.character(field), as.character(q))
          }
          if (!is.null(data)) {
            search_result <- data %>%
              filter(has_matching(uid)) %>%
              head(100) %>%
              transmute(
                name = paste0(
                  uid,
                  "<div class='ui ",
                  case_when(
                    type == "subject_id" ~ "blue",
                    type == "hadm_id" ~ "teal",
                    .default = "yellow"
                  ),
                  " label'>",
                  type,
                  "</div>"
                ),
                value = uid
              )
          }
        }

        search_api_url_uid <-
          custom_register_search(session, uid_list(), search_api_uid, "search_api_uid")

        output$select_uid <- renderUI({
          if (!is.reactive(cohort_id) || isTruthy(cohort_id)) {
            http_get_args <- parseQueryString(session$clientData$url_search)
            if (!is.null(http_get_args[['subject_id']]) &
                !is.null(http_get_args[['hadm_id']])) {
              selectInput(ns("subject_id"),
                          "",
                          http_get_args[['subject_id']],
                          http_get_args[['subject_id']])
            } else{
              form(field(
                tags$label(
                  "Direct search with a UID (subject_id,hadm_id or stay_id)"
                ),
                search_selection_api(ns("uid"), search_api_url_uid, multiple = FALSE)
              ))

            }
          }

        })

        shinyjs::runjs(
          paste0(
            "
                      Shiny.addCustomMessageHandler('",
            ns("select_uid"),
            "', function(value) {
                        data = value.split('&')
                      console.log(data)
                      console.log('",
            ns("subject_id"),
            "')
                      if(data.length > 0 && data[0].length>0){
                        Shiny.setInputValue('",
            ns("subject_id"),
            "', data[0]);
                      $('.",
            ns("subject_id"),
            " > .text').removeClass('default');
                      $('.",
            ns("subject_id"),
            " > .text').text(data[0]);
                      }
                      if(data.length > 1 && data[1].length>0){
                        Shiny.setInputValue('",
            ns("hadm_id"),
            "', data[1]);
                      }
                      if(data.length > 2 && data[2].length>0){
                        Shiny.setInputValue('",
            ns("stay_id"),
            "', data[2]);
                      }

                      });"
          )
        )

        observe({
          if (isTruthy(input$uid) && !is.null(uid_list())) {
            type <- uid_list() %>% filter(uid == !!input$uid) %>% select(type)
            if (type == "subject_id") {
              session$sendCustomMessage(ns("select_uid"), input$uid)
            } else if (type == "hadm_id") {
              selected <-
                dplyr::tbl(database(),
                           in_schema("mimiciv_hosp", "admissions")) %>%
                filter(hadm_id == !!input$uid) %>%
                select(subject_id, hadm_id) %>%
                collect()

              session$sendCustomMessage(ns("select_uid"),
                                        paste(selected[['subject_id']], selected[['hadm_id']], sep = "&"))

            } else if (type == "stay_id") {
              selected <-
                dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays")) %>%
                filter(stay_id == !!input$uid) %>%
                select(subject_id, hadm_id, stay_id) %>%
                collect()
              session$sendCustomMessage(ns("select_uid"),
                                        paste(selected[['subject_id']], selected[['hadm_id']], selected[['stay_id']], sep =
                                                "&"))
            }
          }
        })

        #-------------------------------------------------------------------------------------------------------#
        ####
        ####  Patient Selection - subject_id + hadm_id
        ####
        #-------------------------------------------------------------------------------------------------------#
        subject_id_list <- reactive({
          if (isTruthy(cohort_id) && cohort_id == -1) {
            subject_id_list <-
              dplyr::tbl(database(), in_schema("mimiciv_hosp", "patients")) %>%
              select("subject_id") %>%
              collect()
          } else if (isTruthy(cohort_id) &&
                     cohort_id != -1) {
            subject_id_list <-
              dplyr::tbl(database(), in_schema("public", "cohort")) %>%
              filter(cohort_id == !!cohort_id) %>% select("subject_id") %>% distinct() %>%
              collect()

          } else{
            subject_id_list <- NULL
          }
          subject_id_list
        })



        search_api_subject_id <-
          function(data, q, database) {
            has_matching <- function(field) {
              startsWith(as.character(field), as.character(q))
            }
            if (!is.null(data)) {
              search_result <- data %>%
                filter(has_matching(subject_id)) %>% head(100)
            if(nrow(search_result)>0){
              request <-
                paste0(
                  'select subject_id,gender,COUNT(*) as "total_hadm",coalesce(
                         (select count(*) from mimiciv_hosp.patients i_p
                          join mimiciv_icu.icustays i USING(subject_id)
                          where i_p.subject_id = o_p.subject_id group by subject_id )
                         ,0) as "total_stay",
                         (SELECT i_p.anchor_age + DATE_PART(\'year\', admittime) - anchor_year FROM mimiciv_hosp.admissions i_a natural join mimiciv_hosp.patients i_p
                         WHERE i_a.subject_id = o_p.subject_id ORDER BY admittime LIMIT 1) as "age"
                         from mimiciv_hosp.patients o_p
                       join mimiciv_hosp.admissions a USING(subject_id)
                       where subject_id in (',
                  paste0(search_result[['subject_id']], collapse = ','),
                  ')
                       group by subject_id,gender
                       limit 100'
                )
              dplyr::tbl(database, sql(request)) %>%
                collect() %>%
                transmute(
                  name = paste0(
                    subject_id,
                    "<div class='ui ",
                    case_when(gender == "M" ~ "blue", .default = "olive"),
                    " label'>",
                    gender,
                    "<div class='detail'>",
                    age,
                    "</div></div>",
                    "<div class='ui teal label'>",
                    total_hadm ,
                    "<div class='detail'>HADM</div></div>",
                    case_when(
                      total_stay > 0 ~ paste0(
                        "<div class='ui yellow label'>",
                        total_stay,
                        "<div class='detail'>ICU</div></div>"
                      ),
                      .default = "<div class='ui label'>0<div class='detail'>ICU</div></div>"
                    )
                  ),
                  value = subject_id
                )
            }

            }
          }

        search_api_url_subject_id <- reactive({
          custom_register_search(
            session,
            subject_id_list(),
            search_api_subject_id,
            "search_api_subject_id",
            isolate(database())
          ) #Database call is isolated to prevent usage in a non-reactive env (session$dataObj)
        })


        output$select_patient <- renderUI({
          if (!is.reactive(cohort_id) ||
              isTruthy(cohort_id)) {
            http_get_args <- parseQueryString(session$clientData$url_search)
            if (!is.null(http_get_args[['subject_id']]) &
                !is.null(http_get_args[['hadm_id']])) {
              selectInput(ns("subject_id"),
                          "",
                          http_get_args[['subject_id']],
                          http_get_args[['subject_id']])
            } else{
              form(field(
                tags$label("Enter a subject_id"),
                search_selection_api(
                  ns("subject_id"),
                  search_api_url_subject_id(),
                  multiple = FALSE
                )
              ),
              class = "mb-10")
            }
          } else{
            message_box(
              "No cohort is selected",
              "Please first select a cohort to access exploration tools",
              class = "info my-10"
            )
          }
        })



        output$select_hadm <- renderUI({
          http_get_args <- parseQueryString(session$clientData$url_search)
          if (!is.null(http_get_args[['subject_id']]) &
              !is.null(http_get_args[['hadm_id']])) {
            selectInput(ns("hadm_id"), "", http_get_args[['hadm_id']], http_get_args[['hadm_id']])
          } else{
            req(input$subject_id)
            admissions <-
              dplyr::tbl(database(),
                         in_schema("mimiciv_hosp", "admissions"))
            if (cohort_id == -1) {
              request <-
                paste0(
                  'select hadm_id, admission_type,COUNT(stay_id) as "total_stay"
                             FROM mimiciv_hosp.admissions a
                             left join mimiciv_icu.icustays i USING(subject_id,hadm_id)
                             where subject_id = ',
                  input$subject_id,
                  '
                             group by hadm_id, admission_type
                             limit 100'
                )
              selected_patient_hadm_id_list <-
                dplyr::tbl(database(), sql(request)) %>% collect()

            } else if (isTruthy(cohort_id)) {
              request <-
                paste0(
                  'select hadm_id, admission_type,COUNT(stay_id) as "total_stay"
                             FROM mimiciv_hosp.admissions a
                             JOIN public.cohort USING (subject_id,hadm_id)
                             left join mimiciv_icu.icustays i USING(subject_id,hadm_id,stay_id)
                             where subject_id = ',
                  input$subject_id,
                  ' AND cohort_id = ',
                  cohort_id,
                  '
                             group by hadm_id, admission_type
                             limit 100'
                )

              selected_patient_hadm_id_list <-
                dplyr::tbl(database(), sql(request)) %>% collect()
            } else{
              return(NULL)
            }
            hadm_id_list <-
              selected_patient_hadm_id_list$hadm_id
            labels <-
              lapply(
                paste0(
                  selected_patient_hadm_id_list$hadm_id,
                  " ",
                  selected_patient_hadm_id_list$admission_type,
                  case_when(
                    selected_patient_hadm_id_list$total_stay > 0 ~ paste0(
                      "<div class='ui yellow label'>",
                      selected_patient_hadm_id_list$total_stay,
                      "<div class='detail'>ICU</div></div>"
                    ),
                    .default = "<div class='ui label'>0<div class='detail'>ICU</div></div>"
                  )
                ),
                HTML
              )
            if (length(selected_patient_hadm_id_list$hadm_id) >= 1) {
              if (length(selected_patient_hadm_id_list$hadm_id) == 1) {
                form(field(
                  tags$label("Select a hadm_id"),
                  dropdown_input(
                    ns("hadm_id"),
                    default_text = "Select hospitalization",
                    choices_value = hadm_id_list,
                    choices = labels,
                    value = selected_patient_hadm_id_list$hadm_id[1]
                  )
                ), class = "mb-10")
              } else{
                form(field(
                  tags$label("Select a hadm_id"),
                  dropdown_input(
                    ns("hadm_id"),
                    default_text = "Select hospitalization",
                    choices_value = hadm_id_list,
                    choices = labels,
                    value = isolate(input$hadm_id)
                  )
                ), class = "mb-10")
              }
            } else{
              tagList(
                message_box("", "This subject_id is not linked to any admissions"),
                hidden(numeric_input(ns(
                  "hadm_id"
                ), "", -1))
              )
            }
          }
        })


        output$select_stay <-
          renderUI({
            http_get_args <- parseQueryString(session$clientData$url_search)
            if (!is.null(http_get_args[['subject_id']]) &
                !is.null(http_get_args[['hadm_id']])) {
              if (!is.null(http_get_args[['stay_id']])) {
                shiny.semantic::dropdown_input(ns("stay_id"),
                                               choices = http_get_args[['stay_id']],
                                               value = http_get_args[['stay_id']])
              } else{
                icustays <-
                  dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays"))
                selected_hadm_stay_id_list <-
                  icustays %>%
                  filter(subject_id == !!http_get_args[['subject_id']] &
                           hadm_id == !!http_get_args[['hadm_id']]) %>%
                  select("stay_id") %>%
                  collect()
                labels <-
                  c(
                    "All stays are displayed - Select stay (optional)",
                    selected_hadm_stay_id_list$stay_id
                  )
                stay_id_list <-
                  c("-1", selected_hadm_stay_id_list$stay_id)
                form(field(
                  tags$label("Select a stay_id"),
                  shiny.semantic::dropdown_input(
                    ns("stay_id"),
                    choices_value = stay_id_list,
                    choices = labels,
                    value = -1
                  )
                ),
                class = "mb-10")
              }

            } else{
              req(input$subject_id, input$hadm_id)

              if (cohort_id == -1) {
                selected_hadm_stay_id_list <-
                  dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays")) %>%
                  filter(subject_id == !!input$subject_id &
                           hadm_id == !!input$hadm_id) %>%
                  select("stay_id") %>%
                  collect()
              } else if (isTruthy(cohort_id)) {
                selected_hadm_stay_id_list <-
                  dplyr::tbl(database(), in_schema("public", "cohort")) %>%
                  filter(
                    subject_id == !!input$subject_id &&
                      hadm_id == !!input$hadm_id &&
                      cohort_id == !!cohort_id
                  ) %>%
                  select("stay_id") %>%
                  collect()
              } else{
                return(NULL)
              }

              labels <-
                c(
                  "All stays are displayed - Select stay (optional)",
                  selected_hadm_stay_id_list$stay_id
                )
              stay_id_list <-
                c("-1", selected_hadm_stay_id_list$stay_id)

              if (length(stay_id_list) > 1) {
                if (length(selected_hadm_stay_id_list$stay_id) == 1) {
                  form(field(
                    tags$label("Select a stay_id"),
                    shiny.semantic::dropdown_input(
                      ns("stay_id"),
                      choices_value = stay_id_list,
                      choices = labels,
                      value = selected_hadm_stay_id_list$stay_id[1]
                    )
                  ),
                  class = "mb-10")
                } else{
                  form(field(
                    tags$label("Select a stay_id"),
                    shiny.semantic::dropdown_input(
                      ns("stay_id"),
                      choices_value = stay_id_list,
                      choices = labels,
                      value = ifelse(
                        isTruthy(isolate(input$stay_id)) &&
                          isolate(input$stay_id) %in% stay_id_list,
                        isolate(input$stay_id),
                        -1
                      ),
                    )
                  ),
                  class = "mb-10")
                }
              }
              else{
                tagList(
                  message_box("", "This hadm is not linked to any ICU stay"),
                  hidden(numeric_input(ns(
                    "stay_id"
                  ), "", -1))
                )
              }
            }
          })


        all_events <- reactive({
          http_get_args <- parseQueryString(session$clientData$url_search)
          if (!is.null(http_get_args[['subject_id']]) &
              !is.null(http_get_args[['hadm_id']])) {
            withProgress(message = "Generating request", {
              setProgress(value = 3 / 5, message = "Requesting database - expected waiting time : 5 sec")
              start.time <- Sys.time()

              if (is.null(http_get_args[['stay_id']])) {
                events <-
                  request_compressed_events(database(), http_get_args[['subject_id']], http_get_args[['hadm_id']])

              } else {
                events <-
                  request_compressed_events(database(),
                                            http_get_args[['subject_id']],
                                            http_get_args[['hadm_id']],
                                            http_get_args[['stay_id']])
              }

              end.time <- Sys.time()
              showNotification(paste(
                "Query performance ",
                as.character(end.time - start.time)
              ))
            })
          } else{
            if (isTruthy(input$subject_id) &
                isTruthy(input$hadm_id) &
                isTruthy(input$stay_id)) {
              withProgress(message = "Generating request", {
                setProgress(value = 3 / 5, message = "Requesting database - expected waiting time : 5 sec")
                start.time <- Sys.time()

                if (input$hadm_id == -1) {
                  events <- request_compressed_events(database(), input$subject_id)
                } else if (input$stay_id == -1) {
                  events <-
                    request_compressed_events(database(), input$subject_id, input$hadm_id)

                } else {
                  events <-
                    request_compressed_events(database(),
                                              input$subject_id,
                                              input$hadm_id,
                                              input$stay_id)
                }

                end.time <- Sys.time()
                showNotification(paste(
                  "Query performance ",
                  as.character(end.time - start.time)
                ))
              })
            }

          }

          req(exists("events"))
          return(events)
        })

        hadm_info <- reactive({
          dplyr::tbl(database(), in_schema("mimiciv_hosp", "admissions")) %>%
            filter(subject_id == !!input$subject_id &
                     hadm_id == !!input$hadm_id) %>% collect()
        })

        stay_info <- reactive({
          req(all_events())
          if (input$stay_id != -1) {
            dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays")) %>%
              filter(
                subject_id == !!input$subject_id &
                  hadm_id == !!input$hadm_id &
                  stay_id == !!input$stay_id
              ) %>% collect()
          }
        })

        #-------------------------------------------------------------------------------------------------------#
        ####
        ####  Patient General Information
        ####
        #-------------------------------------------------------------------------------------------------------#

        patient_general_information <- reactiveVal()
        icd_data <- reactiveVal()
        services_data <- reactiveVal()


        output$summary <- renderUI({
          req(all_events())

          patients <-
            dplyr::tbl(database(), in_schema("mimiciv_hosp", "patients"))
          admissions <-
            dplyr::tbl(database(), in_schema("mimiciv_hosp", "admissions"))
          icustays <-
            dplyr::tbl(database(), in_schema("mimiciv_icu", "icustays"))

          transfers <-
            dplyr::tbl(database(), in_schema("mimiciv_hosp", "transfers"))
          if(IS_ED_LOADED){
            triage <-
              dplyr::tbl(database(), in_schema("mimiciv_ed", "triage"))
          }
          diagnoses_icd <-
            dplyr::tbl(database(),
                       in_schema("mimiciv_hosp", "diagnoses_icd"))
          d_icd_diagnoses <-
            dplyr::tbl(database(),
                       in_schema("mimiciv_hosp", "d_icd_diagnoses"))

          if (DATABASE_MODE != "DEMO") {
            charlson <-
              dplyr::tbl(database(),
                         in_schema("mimiciv_derived", "charlson"))
            first_day_gcs <-
              dplyr::tbl(database(),
                         in_schema("mimiciv_derived", "first_day_gcs"))
            first_day_sofa <-
              dplyr::tbl(database(),
                         in_schema("mimiciv_derived", "first_day_sofa"))
          }


          patient_general_information(
            patients %>%
              filter(subject_id == !!input$subject_id) %>%
              inner_join(admissions, by = "subject_id") %>%
              filter(hadm_id == !!input$hadm_id) %>%
              left_join(icustays, by = c("subject_id", "hadm_id")) %>%
              {
                if (DATABASE_MODE != "DEMO") {
                  left_join(., charlson, by = c("subject_id", "hadm_id")) %>%
                    left_join(first_day_sofa,
                              by = c("subject_id", "hadm_id", "stay_id")) %>%
                    left_join(first_day_gcs, by = c("subject_id", "stay_id"))
                } else{
                  .
                }
              } %>%
              mutate(
                age = anchor_age + DATE_PART('year', admittime) - anchor_year,
                hadm_length = AGE(dischtime, admittime),
                stay_length = AGE(outtime, intime),
                in_hospital_death = dod <= dischtime,
                time_to_death = AGE(dod, dischtime)
              ) %>%
              collect()
          )

          divList <- list()

          divList <- append(divList, list(div(
            span(style = "font-weight:bold;", "Gender"),
            paste(" :", patient_general_information()[['gender']][1], sep =
                    " ")
          )))

          divList <- append(divList, list(div(
            span(style = "font-weight:bold;", "Admission Age"),
            paste(" :", patient_general_information()[['age']][1], sep =
                    " ")
          )))
          if (DATABASE_MODE == "DEMO") {
            divList <- append(divList, list(div(
              span(
                style = "font-weight:bold;",
                "Charlson/First 24h SOFA/First 24h minimum Glasgow"
              ),
              paste(" : data not computed in demo version", sep =
                      " ")
            )))
          } else{
            divList <- append(divList, list(div(
              span(style = "font-weight:bold;", "Charlson"),
              paste(" :", patient_general_information()[['charlson_comorbidity_index']][1], sep =
                      " ")
            )))

            divList <- append(divList, list(div(
              span(style = "font-weight:bold;", "First 24h SOFA"),
              paste(" :", patient_general_information()[['sofa']][1], sep =
                      " ")
            )))

            divList <- append(divList, list(div(
              span(style = "font-weight:bold;", "First 24h minimum Glasgow"),
              paste(" :", patient_general_information()[['gcs_min']][1], sep =
                      " ")
            )))
          }


          divList <- append(divList, list(div(
            span(style = "font-weight:bold;", "Hospital Stay Length"),
            paste(" :", patient_general_information()[['hadm_length']][1], sep =
                    " ")
          )))
          http_get_args <-
            parseQueryString(session$clientData$url_search)
          if (input$stay_id != -1 ||
              nrow(patient_general_information()) == 1 ||
              !is.null(http_get_args[["stay_id"]])) {
            divList <- append(divList, list(div(
              span(style = "font-weight:bold;", "ICU Stay Length"),
              paste(" :", patient_general_information()[['stay_length']][1], sep =
                      " ")
            )))
          } else{
            divList <- append(divList, list(div(
              span(style = "font-weight:bold;", "ICU Stay Length"),
              paste(" : Select only one stay to get stay Length", sep =
                      " ")
            )))
          }
          divList <- append(divList, list(div(
            span(style = "font-weight:bold;", "In hospital death"),
            paste(
              " :",
              ifelse(
                !is.na(patient_general_information()[['in_hospital_death']][1]) &
                  patient_general_information()[['in_hospital_death']][1],
                "Yes",
                "No"
              )
            )
          )))

          divList <- append(divList, list(div(
            span(style = "font-weight:bold;", "Date of death"),
            paste(" :", ifelse(
              is.na(patient_general_information()[['dod']][1]),
              "No data (>1y after hadm)",
              paste(
                patient_general_information()[['dod']][1],
                "(time after hospital discharge ",
                patient_general_information()[['time_to_death']][1],
                ")"
              )
            ))
          )))


          icd_data(
            patients %>%
              filter(subject_id == !!input$subject_id) %>%
              inner_join(admissions, by = "subject_id") %>%
              filter(hadm_id == !!input$hadm_id) %>%
              inner_join(diagnoses_icd, by = c("subject_id", "hadm_id")) %>%
              inner_join(d_icd_diagnoses, by = c("icd_code", "icd_version")) %>%
              collect()
          )

          services_data(
            transfers %>%
              filter(subject_id == !!input$subject_id) %>%
              {
                if(IS_ED_LOADED){
                  left_join(.,triage,join_by(transfer_id == stay_id))
                } else{
                  .
                }
              } %>%
              arrange(intime) %>%
              collect()
          )
          icd_div <- apply(icd_data(), 1, function(row) {
            div(tags$b(paste0(row["icd_code"], "v", row["icd_version"], " : ")), row["long_title"])
          })

          event_type_to_td <- function(eventtype){
            if(eventtype == "ED"){
              tags$td(list("ED",icon("ambulance")),class="blue")
            } else if(eventtype == "admit"){
              tags$td(list("Admit",icon("sign in alternate")),class="positive")
            } else if(eventtype == "discharge"){
              tags$td(list("Discharge",icon("sign out alternate")),class="negative")
            } else{
              tags$td(list("Transfer",icon("exchange alternate")),class="warning")
            }
          }
          na_magnifier <- function(text){
            if(is.na(text)){
              tags$span("Unspecified",class="gray")
            } else{
              text
            }
          }
          services_div <- list()
          previous_outtime <- NULL
          current_ED_chiefcomplaint <- services_data()[["chiefcomplaint"]][1]
          current_initial_hospital_transferid <- services_data()[["transfer_id"]][1]
          rowList <- list()
          for (i in 1:nrow(services_data())) {
            current_row <- services_data()[i,]
            if(i!=1 && (is.na(previous_outtime) || current_row[["intime"]] != previous_outtime)){
              services_div <- c(services_div,list(tags$table(
                tags$thead(
                  tags$tr(tags$th(tagList(tags$span(paste0("Hospital Record (",current_initial_hospital_transferid," ",services_data()[["hadm_id"]][i-1],") ",ifelse(!is.na(current_ED_chiefcomplaint),paste(" - ED Chief Complaint :",current_ED_chiefcomplaint),""))),ifelse(services_data()[["hadm_id"]][i-1]==as.numeric(input$hadm_id),list(shiny.semantic::label("Current Record",class="teal right-float")),"")),colspan=4)),
                  tags$tr(
                    tags$th("Event Type"),tags$th("Care Unit"),tags$th("In time"),tags$th("Out time")
                  )
                ),
                tags$tbody(rowList)
                ,class="ui collapsing celled table")))
              rowList <- list()
              if(IS_ED_LOADED){
                current_ED_chiefcomplaint <- ifelse(!is.null(current_row[["chiefcomplaint"]]),current_row[["chiefcomplaint"]],"")
              } else{
                current_ED_chiefcomplaint <- "Unknown - MIMIC-IV ED is not loaded"
              }

              current_initial_hospital_transferid <- current_row[["transfer_id"]]
            }
            rowList <- c(rowList,list(tags$tr(
              event_type_to_td(current_row[["eventtype"]]),
              tags$td(na_magnifier(current_row[["careunit"]])),
              tags$td(na_magnifier(current_row[["intime"]])),
              tags$td(na_magnifier(current_row[["outtime"]]))
            )))
            previous_outtime <- current_row[["outtime"]]
          }
          #Closing last opened Hospital Record
          services_div <- c(services_div,list(tags$table(
            tags$thead(
              tags$tr(tags$th(tagList(tags$span(paste0("Hospital Record (",current_initial_hospital_transferid," ",services_data()[["hadm_id"]][i-1],") ",ifelse(!is.na(current_ED_chiefcomplaint),paste(" - ED Chief Complaint :",current_ED_chiefcomplaint),""))),ifelse(services_data()[["hadm_id"]][i-1]==as.numeric(input$hadm_id),list(shiny.semantic::label("Current Record",class="teal right-float")),"")),colspan=4)),
              tags$tr(
                tags$th("Event Type"),tags$th("Care Unit"),tags$th("In time"),tags$th("Out time")
              )
            ),
            tags$tbody(rowList)
            ,class="ui collapsing celled table")))


          accordion_content <-
            list(
              list(title = "Diagnoses (by ICD Code)", content = icd_div),
              list(title = "Patient services history", content = services_div)
            )



          tagList(
            div(
              class = "ui segment",
              div(div(
                div(class = "ui header", "Patient General Information"),
                tagList(divList)
              ), style = "display:flex; flex-direction:row")
            ),
            accordion(
              accordion_content,
              active_title = "Diagnoses (by ICD Code)",
              fluid = TRUE
            )
          )

        })
        #-------------------------------------------------------------------------------------------------------#
        ####
        ####  Timeline Creation
        ####
        #-------------------------------------------------------------------------------------------------------#

        # Item id to show on timeline

        timeline_itemid_list <-
          reactiveValues(data = unlist(default_itemid()))

        observe({
          timeline_itemid_list$data <- unlist(default_itemid())
        })

        # Create item remove onclick button event on client side

        shinyjs::runjs(
          paste0(
            "$('body').on('click', '.timeline_parameter.close', function() {let icon_id = $(this).attr('id');Shiny.setInputValue('",
            ns('item_to_remove'),
            "', Date.now() +'@'+ icon_id.split('-').pop());});"
          )
        )


        output$hadm_preview <- renderUI({
          if (isTruthy(patient_general_information()) &&
              isTruthy(input$subject_id)) {
            http_get_args <- parseQueryString(session$clientData$url_search)
            tagList(
              tags$div(
                tags$div(
                  icon("hospital"),
                  div(
                    div("HADM Admission", class = "title"),
                    div(patient_general_information()[["admittime"]][1], class =
                          "description")
                    ,
                    class = "content"
                  )
                  ,
                  class = "step"
                ),
                tags$div(div(
                  div("HADM Discharge", class = "title"),
                  div(patient_general_information()[["dischtime"]][1], class =
                        "description")
                  ,
                  class = "content"
                )
                , class = "step")
                ,
                class = "ui small steps mr-10 my-10"
              ),
              if (input$stay_id != -1 ||
                  nrow(patient_general_information()) == 1 ||
                  !is.null(http_get_args[["stay_id"]])) {
                tags$div(
                  tags$div(
                    icon("procedures"),
                    div(
                      div("ICU In Time", class = "title"),
                      div(patient_general_information()[["intime"]][1], class =
                            "description")
                      ,
                      class = "content"
                    )
                    ,
                    class = "step"
                  ),
                  tags$div(div(
                    div("ICU Out Time", class = "title"),
                    div(patient_general_information()[["outtime"]][1], class =
                          "description")
                    ,
                    class = "content"
                  )
                  , class = "step")
                  ,
                  class = "ui small steps mr-10 my-10"
                )
              }
            )
          }
        })

        # Select parameter input generation
        output$timeline_parameter_selection <-
          renderUI({
            req(all_events())
            events <- all_events() %>%
              select("label", "category","param_type" , "itemid") %>%
              distinct() %>%
              filter(!(itemid %in% timeline_itemid_list$data)) %>%
              pivot_wider(
                names_from = c(label, param_type, category, itemid),
                names_glue = "{category} - {param_type} {label} ({itemid})",
                values_from = itemid
              )
            choices <- events
            search_input <-
              custom_search_selection_choices(ns("add_timeline_parameters"), choices, multiple = TRUE)
            form(fields(
              field(
                class = "twelve wide",
                tags$label("Select the parameters you want to draw"),
                search_input
              ),
              field(
                class = "four wide",
                tags$label("‎"),
                button(
                  ns("add_to_timeline_button"),
                  label = "",
                  icon = icon("plus"),
                  class = "icon"
                ),
                button(
                  ns("create_both_t_button"),
                  label = "",
                  icon = tags$i(icon("plus"), icon("collapsed plus")) ,
                  class = "icon"
                )
              )

            ))
          })


        # Click on add on timeline button handler

        observeEvent(input$add_to_timeline_button, {
          new_parameters <-
            as.integer(unlist(strsplit(
              input$add_timeline_parameters, ","
            )))
          timeline_itemid_list$data <-
            c(timeline_itemid_list$data, new_parameters)
          shinyjs::runjs(paste0(
            "$('",
            ns("add_timeline_parameters"),
            "').dropdown('clear');"
          ))
        })

        #Click on create both button handler
        observeEvent(input$create_both_t_button, {
          new_parameters <-
            as.integer(unlist(strsplit(
              input$add_timeline_parameters, ","
            )))
          timeline_itemid_list$data <-
            c(timeline_itemid_list$data, new_parameters)
          shinyjs::runjs(paste0(
            "$('",
            ns("add_timeline_parameters"),
            "').dropdown('clear');"
          ))
          if (length(new_parameters) > 0) {
            card_list$data <-
              c(list(new_parameters), card_list$data)
          }
        })

        # Display currently selected parameter for timeline display

        output$timeline_parameter_display <- renderUI({
          req(all_events())
          events <-
            all_events() %>% filter(itemid %in% timeline_itemid_list$data) %>% select(itemid, category, label) %>% distinct() %>% collect()
          if (nrow(events) > 0) {
            segment(apply(events, 1, function(item) {
              div(
                paste0(item["category"], " - ", item["label"]),
                icon(
                  id = paste0("tm-item-remove-", item["itemid"]),
                  class = "timeline_parameter close"
                ),
                class = paste0("ui label")
              )
            }))
          }
        })

        # Item remove onclick button event observer

        observeEvent(input$item_to_remove, {
          item_id <- as.integer(strsplit(input$item_to_remove,"@")[[1]][2])
          timeline_itemid_list$data <-
            timeline_itemid_list$data[timeline_itemid_list$data != item_id]
        })

        timeline_data <- reactiveVal()
        category_to_merge <-
          list(
            "General",
            "Routine Vital Signs",
            "Chemistry",
            "Hematology",
            "Labs",
            "Neurological",
            "Hematology",
            "Restraint/Support Systems",
            "Access Lines - Peripheral",
            "Skin - Assessment",
            "GI/GU",
            "Care Plans",
            "Microbiology"
          )

        # Timeline render
        output$timeline <- renderUI({
          req(all_events())
          timeline_data(
            all_events() %>%
              filter(itemid %in% timeline_itemid_list$data) %>%
              mutate(id = row_number())
          )
          timeline(
            timeline_data(),
            start = "starttime",
            end  = "endtime",
            content = "label",
            elementId = ns("selected_patient_timeline"),
            category_to_merge = category_to_merge
          )
        })

        selected_event <- reactiveVal(NULL)

        observe({
          if (!is.null(input$selected_patient_timeline_selected)) {
            previous_selected <- isolate(selected_event())
            if (is.null(previous_selected) ||
                previous_selected != input$selected_patient_timeline_selected) {
              selected_event(input$selected_patient_timeline_selected)
              runjs("$('.right-side-div.timevis-details').addClass('visible')")
            }

          } else{
            selected_event(NULL)
            runjs("$('.right-side-div.timevis-details').removeClass('visible')")
          }

        })

        output$timevis_details_ui <- renderUI({
          if (!is.null(selected_event())) {
            selected_item <-
              timeline_data() %>% filter(id == !!selected_event())
            if (selected_item[["category"]] %in% category_to_merge) {
              merged_items <-
                timeline_data() %>% filter(starttime == selected_item[["starttime"]] &
                                             category == selected_item[["category"]])
              items_html <- list()
              for (i in 1:nrow(merged_items)) {
                item <- merged_items[i, ]
                item_in_db <-
                  request_event_from_data(database(),
                                          item[["subject_id"]],
                                          item[["hadm_id"]],
                                          item[["stay_id"]],
                                          item[["itemid"]],
                                          item[["linksto"]],
                                          item[["starttime"]],
                                          item[["endtime"]],
                                          item[["value"]],
                                          item[["valueuom"]])
                items_html <-
                  tagList(items_html,
                          events_magnifier(item_in_db, item[["linksto"]]))
              }
              if (nrow(merged_items) > 1) {
                tagList(message_box(
                  "",
                  paste(
                    "This timeline item contains",
                    nrow(merged_items),
                    "items"
                  ),
                  class = "info my-10"
                ),
                items_html)
              } else{
                items_html
              }

            } else{
              item_in_db <- request_event_from_data(
                database(),
                selected_item[["subject_id"]],
                selected_item[["hadm_id"]],
                selected_item[["stay_id"]],
                selected_item[["itemid"]],
                selected_item[["linksto"]],
                selected_item[["starttime"]],
                selected_item[["endtime"]],
                selected_item[["value"]],
                selected_item[["valueuom"]]
              )
              events_magnifier(item_in_db, selected_item[["linksto"]])
            }
          }
        })

        #-------------------------------------------------------------------------------------------------------#
        ####
        ####  Parameter Tracking - Item specific graph generation
        ####
        #-------------------------------------------------------------------------------------------------------#

        # Select parameter to display input generation

        output$cards_parameter_selection <- renderUI({
          req(all_events())
          events <- all_events() %>%
            select("label", "category","param_type", "itemid") %>%
            distinct() %>%
            pivot_wider(
              names_from = c(label, param_type, category, itemid),
              names_glue = "{category} - {param_type} {label} ({itemid})",
              values_from = itemid
            )
          choices <- events
          search_input <-
            custom_search_selection_choices(ns("create_diagram_parameters"), choices, multiple = TRUE)
          form(fields(
            field(
              class = "twelve wide",
              tags$label("Select the parameters you want to draw"),
              search_input
            ),
            field(
              class = "four wide",
              tags$label("‎"),
              button(
                ns("create_diagram_button"),
                label = "",
                icon = icon("plus"),
                class = "icon"
              ),
              button(
                ns("create_both_g_button"),
                label = "",
                icon = tags$i(icon("plus"), icon("collapsed plus")) ,
                class = "icon"
              )
            )

          ))
        })

        # Default cards to show

        default_cards <- default_itemid()

        # Reactive container for itemid in cards

        card_list <-
          reactiveValues(data = default_cards)

        observe({
          card_list$data <- default_itemid()
        })

        # Create diagram click observer

        observeEvent(input$create_diagram_button, {
          new_parameters <-
            as.integer(unlist(strsplit(
              input$create_diagram_parameters, ","
            )))
          if (length(new_parameters) > 0) {
            card_list$data <-
              c(list(new_parameters), card_list$data)
          }

        })

        #Click on create both button handler
        observeEvent(input$create_both_g_button, {
          new_parameters <-
            as.integer(unlist(strsplit(
              input$create_diagram_parameters, ","
            )))
          timeline_itemid_list$data <-
            c(timeline_itemid_list$data, new_parameters)
          shinyjs::runjs(paste0(
            "$('",
            ns("add_timeline_parameters"),
            "').dropdown('clear');"
          ))
          if (length(new_parameters) > 0) {
            card_list$data <-
              c(list(new_parameters), card_list$data)
          }
        })

        downloadCache <- reactiveVal(NULL)

        # Cards (graph container) + graph generation
        output$cards <- renderUI({
          req(all_events())
          html_element_list <- list()
          trace_data <- list()
          rendered_plot_list <- list()
          cards_to_generate <- card_list$data
          downloadCache(NULL)
          skippedPlot <- 0
          for (identifiers in cards_to_generate) {
            plot_id = paste(c("plot", identifiers), collapse = "_")
            trace_data[[plot_id]] <- all_events() %>%
              filter(itemid %in% identifiers) %>%
              collect()
            if (nrow(trace_data[[plot_id]]) == 0) {
              skippedPlot <- skippedPlot + 1
              next
            }
            if(all(!is.na(as.numeric(na.omit(trace_data[[plot_id]]$value))))){
              trace_data[[plot_id]]$value <- as.numeric(trace_data[[plot_id]]$value)
            }
            isolate({
              downloadCache(c(downloadCache(), list(trace_data[[plot_id]])))
            })

            card_info <- tibble()
            hadm_info()
            stay_info()

            for (current_itemid in identifiers) {
              card_info <- bind_rows(card_info,
                                     list(
                                       all_events() %>%
                                         filter(itemid == current_itemid) %>%
                                         head(1) %>%
                                         select("category", "label", "abbreviation", "valueuom") %>%
                                         collect()
                                     ))
            }
            rendered_plot_list[[plot_id]] <- local({
              local_plot_id <- plot_id
              vline <-
                function(x = 0,
                         color = "black") {
                  list(
                    type = "line",
                    y0 = 0,
                    y1 = 1,
                    yref = "paper",
                    x0 = x,
                    x1 = x,
                    line = list(color = color, dash = "dot")
                  )
                }
              renderPlotly({
                trace_data[[local_plot_id]]$text <-
                  round(trace_data[[local_plot_id]]$value, 3)
                plot <- plot_ly(type = "scatter",
                                mode = "lines+markers",
                                source = local_plot_id)
                labels <-
                  unique(trace_data[[local_plot_id]]$label)
                yaxis_layout <- list()
                for (idx in seq_along(labels)) {
                  filtered_data <-
                    trace_data[[local_plot_id]] %>% filter(label == labels[idx])
                  plot <- plot %>% add_trace(
                    x = filtered_data$starttime,
                    y =  filtered_data$value,
                    showlegend = T,
                    name = filtered_data$label[1],
                    text = filtered_data$text,
                    yaxis = paste0("y", idx),
                    marker = list(color = brewer.pal(8, "Set2")[idx]),
                    line = list(color = brewer.pal(8, "Set2")[idx])

                  )
                  yaxis_layout[[paste0("yaxis", idx)]] <-
                    list(
                      overlaying = "y",
                      side = "left",
                      title = paste0("<b>", labels[idx], "</b>"),
                      position = 0.05 * (idx - 1),
                      tickfont = list(color = brewer.pal(8, "Set2")[idx]),
                      titlefont = list(color = brewer.pal(8, "Set2")[idx])
                    )
                }

                plot <-
                  plot %>% layout(
                    xaxis = list(
                      range = c(stay_info()$intime, stay_info()$outtime),
                      domain = c(idx * 0.05, 1)
                    ),
                    yaxis = list(
                      title = paste0("<b>", labels[1], "</b>"),
                      tickfont = list(color = brewer.pal(8, "Set2")[1]),
                      titlefont = list(color = brewer.pal(8, "Set2")[1])
                    ),

                    shapes = list(
                      vline(stay_info()$intime, "red"),
                      vline(stay_info()$outtime, "red"),
                      vline(hadm_info()$admittime, "green"),
                      vline(hadm_info()$dischtime, "green")
                    )
                  )
                plot <-
                  do.call(layout, c(list(p = plot), yaxis_layout))
                plot
              })
            })

            if (nrow(card_info[, "abbreviation"]) > 0) {
              for (i in 1:nrow(card_info[, "abbreviation"])) {
                if (length(card_info[i, "abbreviation"]) > 0 &&
                    is.na(as.numeric(card_info[i, "abbreviation"]))) {
                  card_info[i, "abbreviation"] <- card_info[i, "label"]
                }
              }
            }
            title <-
              sapply(card_info[, "abbreviation"], paste, collapse = "/")
            subtitle <-
              sapply(card_info[, "category"], paste, collapse = "/")

            desc_by_item <-
              mapply(function(x, y)
                paste(x, "(", y, ")", sep = ""),
                card_info[, "label"],
                card_info[, "valueuom"])
            description <-
              paste(desc_by_item, collapse = " / ")
            extra <-
              div(
                downloadLink(ns(
                  paste0("plot_download_data_", plot_id)
                ), label = icon("link download download-data")),
                icon(paste0(
                  "link expand alternate expand-diagram ",
                  ns("plot_to_expand")
                )),
                icon(
                  paste0(
                    "link trash alternate remove-diagram ",
                    ns("plot_to_remove")
                  ),
                  id = plot_id
                ),
                class = "right floated meta"
              )


            html_element_list <-
              append(html_element_list,
                     list(
                       htmltools::tagAppendAttributes(
                         createCard(
                           rendered_plot_list[[plot_id]],
                           title,
                           subtitle,
                           description,
                           extra
                         ),
                         class = "expanded"
                       )
                     ))
          }

          tagList(cards(html_element_list), {
            if (skippedPlot > 0)
              message_box(
                "Missing data",
                paste(
                  skippedPlot,
                  "plot have been skipped because of missing data"
                ),
                class = "info my-10",
                closable = T
              )
          })

        })



        observe({
          trace_data <- list()
          index <- 1
          for (identifiers in card_list$data) {
            plot_id <- paste(c("plot", identifiers), collapse = "_")

            output[[paste0("plot_download_data_", plot_id)]] <-
              local({
                local_plot_id <- plot_id
                local_index <- index
                downloadHandler(
                  filename = function() {
                    paste0("custom_export_", local_plot_id, ".csv")
                  },
                  content = function(file) {
                    write.csv(downloadCache()[[local_index]], file, row.names = FALSE)
                  }
                )
              })
            index <- index + 1
          }
        })

        # close right popup when click outside of the div
        runjs(
          paste0(
            "$('body').on('click','#",
            ns("patient-explorer-container"),
            "', function(e){
                        if(!$(e.target).hasClass('vis-item') && $(e.target).closest('.right-side-div').length == 0){
                        console.log(e.target)
                          $('.right-side-div.timevis-details').removeClass('visible')
                        }
                     })"
          )
        )

        # expand-diagram icon event handler

        shinyjs::runjs(
          paste0(
            "$('body').on('click', '.link.expand-diagram.",
            ns("plot_to_expand"),
            "', function() {
                              let currentCard =  $(this).closest('.ui.card')
                                currentCard.toggleClass('expanded');
                                let plotId = currentCard.find('.plotly').attr('id');
                       console.log(plotId)
                                  Plotly.Plots.resize(plotId);
                                });"
          )
        )

        # Create diagram remove onclick button event on client side

        shinyjs::runjs(
          paste0(
            "$('body').on('click', '.trash.remove-diagram.",
            ns("plot_to_remove"),
            "', function() {
                              let plotId = $(this).attr('id');
                       console.log(plotId);
                              Shiny.setInputValue('",
            ns('plot_to_remove'),
            "', plotId);});"
          )
        )

        # Observe diagram remove onclick button event on server side

        observeEvent(input$plot_to_remove, {
          identifier <-
            as.integer(unlist(strsplit(input$plot_to_remove, "_"))[-1])

          bool_filter <-
            sapply(card_list$data, function(x)
              all(x == identifier))
          index <- which(bool_filter)
          if (index != 0) {
            card_list$data <- card_list$data[-index]
          } else{
            print(
              paste0(
                "Can't find plot_to_remove (id:",
                identifier,
                ") in ",
                card_list$data
              )
            )
          }
        }, domain = session)

      }
    })




  }
