userProfileServer <- function(id, database = NULL) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 if (!is.null(database)) {

                   selected_profile <- session$userData$selected_profile

                   invalidator <- reactiveVal(0)
                   # Select parameter input generation


                   output$add_user_parameter_ui <-
                     renderUI({

                       search_input <- search_selection_api(ns("add_user_parameter"), search_api_url_distinct_events, multiple = TRUE)
                       div(field(
                           class = "sixteen wide",
                           tags$label("Select the parameters you want to observe with this profile"),
                           search_input
                         ), class="my-10")
                     })

                   output$add_user_cohort_ui <-
                     renderUI({
                       choices <-
                         dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>%
                         select("cohort_id", "cohort_name") %>%
                         collect() %>%
                         pivot_wider(
                           names_from = cohort_name,
                           values_from = cohort_id
                         )
                       search_input <-
                         custom_search_selection_choices(ns("add_user_cohort"),
                                                         choices,
                                                         multiple = TRUE)
                       div(field(
                         class = "sixteen wide",
                         tags$label("Select the cohort you want to observe with this profile"),
                         search_input
                       ), class="my-10")
                     })

                   output$add_user_result <- eventReactive(input$add_user_action,{
                     user_names <-
                       dplyr::tbl(database(), in_schema("public", "users")) %>% select("user_name") %>% collect()
                     if (!(input$add_user_name %in% as.list(user_names)$user_name)) {
                       user_data <-
                         data.frame(
                           user_name = c(input$add_user_name),
                           user_description = c(input$add_user_desc),
                           user_itemids = c(input$add_user_parameter),
                           user_cohorts = c(input$add_user_cohort)
                         )
                       row_added <- dbAppendTable(
                         database(),
                         Id(schema = "public", table = "users"),
                         as.data.frame(user_data)
                       )
                       if(row_added==1){
                           isolate(invalidator(invalidator()+1))
                           isolate(selected_profile())
                           updateTextInput(session,"add_user_name", value="")
                           updateTextInput(session,"add_user_desc", value="")
                           shinyjs::runjs("Shiny.setInputValue('update_profile', Date.now());")
                           message_box("",
                                       "Profile successfully added to the application",
                                       class = "success",
                                       closable = T
                           )
                       }else{
                         toast(
                           "A error occured when trying to create the user in database. User persistence has been cancelled",
                           "red"
                         )
                       }
                     } else{
                       toast(
                         "",
                         paste0(
                           "A user named \"",
                           htmlEscape(input$add_user_name),
                           "\" already exist in database. User name should be unique"
                         ),
                         "red"
                       )
                     }
                   })
                   choices_itemid <-
                     dplyr::tbl(database(), in_schema("public", "distinct_events")) %>%
                     select("label", "category", "itemid") %>%
                     collect()

                   choices_itemid_to_select_format <- choices_itemid %>%
                     pivot_wider(
                       names_from = c(label, category, itemid),
                       names_glue = "{category} - {label} ({itemid})",
                       values_from = itemid
                     ) %>% pivot_longer(everything(),names_to = "name", values_to = "value")

                   choices_itemid_to_select_format_json <- toJSON(choices_itemid_to_select_format, pretty = TRUE)

                   output$users_table <- renderUI({
                     invalidator()
                     data_table <- dplyr::tbl(database(),in_schema("public", "users")) %>% collect()


                     choices_cohort <-
                       dplyr::tbl(database(), in_schema("public", "d_cohorts")) %>%
                       select("cohort_id", "cohort_name") %>%
                       collect() %>%
                       pivot_wider(
                         names_from = cohort_name,
                         values_from = cohort_id
                       )

                     tags$table(
                       tags$thead(
                         tags$th("Profile name"),
                         tags$th("Description"),
                         tags$th("Parameters of interest"),
                         tags$th("Cohort of interest"),
                         htmltools::tagAppendAttributes(tags$th("Action"),width="110px")
                       ),
                       tags$tbody(
                         if(nrow(data_table)>0){
                         tagList(apply(data_table,MARGIN = 1,function(row){
                           observeEvent(input[[paste0("delete-",row['user_id'])]], {
                             create_modal(
                               modal(
                                 id = ns(paste0("delete-modal-",row['user_id'])),
                                 header = h2("Delete a user"),
                                 content = paste0("Are you sure you want to delete ",row['user_name']," ?"),
                                 footer = tagList(button(ns(paste0("cancel_delete_",row['user_id'])),label="Cancel"),button(ns(paste0("confirm_delete_",row['user_id'])),label="Confirm delete",class="red"))
                               )
                             )
                             show_modal(ns(paste0("delete-modal-",row['user_id'])))
                           })
                           observeEvent(input[[paste0("cancel_delete_",row['user_id'])]], {
                             hide_modal(ns(paste0("delete-modal-",row['user_id'])))
                           })
                           observeEvent(input[[paste0("confirm_delete_",row['user_id'])]], {
                             query <- "DELETE FROM public.users WHERE user_id = $1"
                             delete <- dbSendQuery(database(), query)
                             dbBind(delete, list(row['user_id']))
                             dbClearResult(delete)


                             hide_modal(ns(paste0("delete-modal-",row['user_id'])))
                             shinyjs::runjs("Shiny.setInputValue('update_profile', Date.now());")
                             invalidator(invalidator()+1)
                             selected_profile()
                           })

                           observeEvent(input[[paste0("save-",row['user_id'])]], {
                             print(paste0("UPDATE public.users SET user_itemids='",input[[paste0("list_user_parameter_",row['user_id'])]],"', user_cohorts='",input[[paste0("list_user_cohort_",row['user_id'])]],"' WHERE user_id = ",row['user_id']))
                             dbSendStatement(database(),sql(paste0("UPDATE public.users SET user_itemids='",input[[paste0("list_user_parameter_",row['user_id'])]],"', user_cohorts='",input[[paste0("list_user_cohort_",row['user_id'])]],"' WHERE user_id = ",row['user_id'])),n=-1)
                             shinyjs::runjs("Shiny.setInputValue('update_profile', Date.now());")
                             invalidator(invalidator()+1)
                             toast(
                               "",
                               "User has been updated",
                               "green"
                             )
                             freezeReactiveValue(input, paste0("save-",row['user_id']))
                           })
                           search_itemid <-
                             custom_search_selection_choices(ns(paste0("list_user_parameter_",row['user_id'])),
                                                             choices_itemid %>% filter(itemid %in% strsplit(row[['user_itemids']],',',fixed=T)[[1]] ) %>%
                                                               pivot_wider(
                                                                 names_from = c(label, category, itemid),
                                                                 names_glue = "{category} - {label} ({itemid})",
                                                                 values_from = itemid
                                                               ),
                                                             multiple = TRUE,
                                                             value = row['user_itemids'],
                                                             class = "disabled")
                           # search_itemid <- custom_search_selection_api(ns(paste0("list_user_parameter_",row['user_id'])),
                           #                                              search_api_url_distinct_events,
                           #                                              multiple = TRUE,
                           #                                              value = row['user_itemids'],
                           #                                              class = "disabled")
                           search_cohort <-
                             custom_search_selection_choices(ns(paste0("list_user_cohort_",row['user_id'])),
                                                             choices_cohort,
                                                             multiple = TRUE,
                                                             value = row['user_cohorts'],
                                                             class = "disabled")

                           shinyjs::runjs(paste0(
                             "
                        $('body').on('click','#",
                        ns(paste0("edit-",row['user_id'])),
                        "',function(e) {
                          $(this).hide()
                          $('#",ns(paste0("save-",row['user_id'])),"').show();
                          $('.",ns(paste0("list_user_parameter_",row['user_id'])),"').dropdown('setup menu', { values:",choices_itemid_to_select_format_json,"});
                          $('.",ns(paste0("list_user_parameter_",row['user_id'])),"').removeClass('disabled');
                          $('.",ns(paste0("list_user_cohort_",row['user_id'])),"').removeClass('disabled');
                        })"))
                           tags$tr(
                             tags$td(row['user_name']),
                             tags$td(row['user_description']),
                             tags$td(search_itemid),
                             tags$td(search_cohort),
                             tags$td(
                                   htmltools::tagAppendAttributes(button(ns(paste0("save-",row['user_id'])),label="",icon = icon("save"),class="olive icon"),style="display:none;"),
                                   button(ns(paste0("edit-",row['user_id'])),label="",icon = icon("edit"),class="yellow icon"),
                                   button(ns(paste0("delete-",row['user_id'])),label="",icon = icon("trash"),class="red icon")


                               )
                           )
                         }))
                         }),
                       class="ui celled table")
                   })

                 }
               })
}
