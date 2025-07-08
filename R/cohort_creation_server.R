cohortCreationServer <- function(id, database = NULL) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 if (!is.null(database)) {

                   selected_profile <- session$userData$selected_profile

                   condition_object <- eventSearchbarServer("event_searchbar", database)


                   # ************************************************************************************#
                   #------------------------------- FETCH COHORT SUMMARY ----------------------------------
                   # ____________________________________________________________________________________#


                   fetched_cohort <- reactiveVal(NULL)

                   output$fetched_details <- renderUI({
                     if (length(condition_object()) != 0) {
                       # Progress 0-1 init 1-4 request, 4-5 apply logic, 5-6 compute details
                       progress <- Progress$new(session, min = 0, max = 7)
                       progress$set(message = 'Generating request')
                       user_condition_object <- condition_object()
                       progress$set(value = 1, message = 'Requesting database')
                       print("Step 1 Generate")
                       promise_list <-
                         request_constrained(user_condition_object, progress)
                       print("Step 2 Resolve")
                       promise_all(.list = promise_list)  %...>% (function(data) {
                       print("Step 3 All is resolved")
                         start.time <- Sys.time()
                         for(key in names(data)){
                           assign(key,data[[key]])
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
                       }) %...!% (function(e){
                         progress$close()
                         message_box(
                           "An error has occured",
                           HTML(paste0("Are you sure every event has a set and meaningful condition ? That all your parenthesis are correct ? <br><br><b>Error detail :</b><br><code>",e,"</code>")),
                           class = "negative my-10",
                           closable = T
                         )

                     })
}
                   })
                   # ************************************************************************************#
                   #---------------------------------- PERSIST COHORT ------------------------------------
                   # ____________________________________________________________________________________#


                   output$persist_action <- renderUI({
                     if (!is.null(fetched_cohort())) {
                       cohort_list <-
                         dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>% collect()
                       n_cohort <- nrow(cohort_list)
                      htmltools::tagAppendAttributes(form(
                         fields(
                           field(
                             tags$label("Enter the cohort name"),
                             text_input(
                               ns("cohort_name"),
                               label = "",
                               value = "",
                               attribs = list(disabled="disabled")
                             ),
                             class = "four wide"
                           ),

                           field(
                             tags$label("Enter the cohort description"),
                             text_input(
                               ns("cohort_desc"),
                               label = "",
                               value = "",
                               attribs = list(disabled="disabled")
                             ),
                             class = "twelve wide"
                           )
                           ,
                           class = "inline"
                         ),
                         button(ns("persist_button"), label = "Persist Cohort"),
                         class = "mt-10"
                       ),style="position:static")
                     }
                   })

                   persistMessage <-
                     eventReactive(input$persist_button, {
                       cohort_list <-
                         dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>% collect()
                       n_cohort <- nrow(cohort_list)
                       if (input$cohort_name != "" && input$cohort_desc != "") {
                         withProgress(message = "Checking if cohort name is unique", {
                           cohort_names <-
                             dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>% select("cohort_name") %>% collect()
                           if (!(input$cohort_name %in% as.list(cohort_names)$cohort_name)) {
                             # Add cohort to d_cohorts
                             d_cohorts_data <-
                               data.frame(
                                 cohort_name = c(input$cohort_name),
                                 cohort_description = c(input$cohort_desc)
                               )
                             setProgress(value = 2 / 5, message = "Cohort description registration")
                             dbAppendTable(database(),
                                           Id(schema = "public", table = "d_cohorts"),
                                           d_cohorts_data)
                             new_cohort_id <-
                               as.numeric(
                                 dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>% filter(cohort_name == !!input$cohort_name) %>% select(cohort_id) %>% head(1) %>% collect()
                               )
                             # Linked patient data to this cohort
                             setProgress(value = 3 / 5, message = "Retrieving data from cache")
                             cohort_data <-
                               fetched_cohort() %>% select(subject_id, hadm_id, stay_id) %>% mutate(cohort_id = new_cohort_id)
                             setProgress(value = 4 / 5, message = "Cohort data persisting")
                             dbAppendTable(
                               database(),
                               Id(schema = "public", table = "cohort"),
                               as.data.frame(cohort_data)
                             )
                             if(!is.null(selected_profile) & selected_profile()$user_id != 0){
                               query <- "UPDATE users SET user_cohorts = user_cohorts || $1 WHERE user_id = $2"
                               update <- dbSendQuery(database(), query)

                               dbBind(update, list(paste0(',',new_cohort_id), selected_profile()$user_id))
                               dbClearResult(update)
                               }

                             toast(
                               "",
                               paste0(
                                 "Your cohort <b>",
                                 htmlEscape(input$cohort_name),
                                 "</b> has been persisted"
                               ),
                               "green"
                             )
                             message_box(
                               "Persist action success",
                               "Your now able to explore your cohort in Cohort Explorer page",
                               class = "positive my-10",
                               closable = T
                             )
                           } else{
                             toast(
                               "",
                               paste0(
                                 "A cohort named \"",
                                 htmlEscape(input$cohort_name),
                                 "\" already exist in database. Cohort name should be unique"
                               ),
                               "yellow"
                             )
                           }
                         })
                       } else{
                         toast("",
                               paste0("A cohort should have a non-empty name and description"),
                               "red")
                       }


                     })

                   output$result_persist <- renderUI({
                     persistMessage()
                   })


                   }
})
}


