devSettingsServer <- function(id, database = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    if (!is.null(database)) {
      file_data <- reactive({
        file_data <- as_tibble(read.csv(input$user_custom_file$datapath, header = T))
      })

      is_previewed <- reactiveVal(FALSE)

      observeEvent(input$user_custom_file, {
        print("file_change")
        is_previewed(FALSE)
      })

      previewResult <- eventReactive(input$user_custom_preview_event_action, {
        print("click")
        is_previewed(TRUE)
        if (input$user_custom_label != "" &
            input$user_custom_abbr != "" & input$user_custom_author != "") {
          required_column <- c("subject_id",
                               "hadm_id",
                               "stay_id",
                               "charttime",
                               "value",
                               "valueuom")
          if (all(names(file_data()) %in% required_column)) {
            check_not_empty <- TRUE
            for (col_name in required_column) {
              x <- file_data()[[col_name]]
              if ((length(x[!is.na(x)]) == 0 |
                   length(x[!is.null(x)]) == 0) & (col_name != "stay_id" & col_name != "valueuom")) {
                check_not_empty <- check_not_empty & FALSE
              }
            }
            if (check_not_empty) {
              tagList(
                message_box(
                  "Custom event preview",
                  "You're going to add these events, please ensure data format is right and your event name is unique",
                  class = "info",
                  closable = T
                ),
                div(
                  button(
                    ns("user_custom_add_event_action"),
                    label = "Persist event in database",
                    class = "teal"
                  ),
                  class = "my-10"
                ),
                {
                  if (is_previewed())
                    renderDataTable(file_data() %>% select_if(names(.) %in% required_column) %>% head(50))
                }
              )
            } else{
              tagList(
                message_box(
                  "Error (empty required column)",
                  "One of the required column is empty, please ensure subject_id,hadm_id,stay_id,charttime,value,valueuom data column are filled",
                  class = "error",
                  closable = T
                ),
                renderDataTable(file_data() %>% head(50))
              )
            }
          } else{
            tagList(
              message_box(
                "Error (missing required column)",
                "One of the required column is missing, please ensure you've provided subject_id,hadm_id,stay_id,charttime,value,valueuom data column",
                class = "error",
                closable = T
              ),
              renderDataTable(file_data() %>% head(50))
            )
          }
        } else{
          message_box(
            "Error (missing required user input)",
            "Custom event should have a non-empty label, abbreviation and author",
            class = "error",
            closable = T
          )
        }
      })


      output$preview_custom_event <- renderUI({
        req(previewResult())
        if (is_previewed()) {
          div(previewResult(), class = "my-10")
        }

      })

      output$persist_custom_event <- renderUI({
        req(persistResult())
        div(persistResult(), class = "my-10")

      })

      persistResult <- eventReactive(input$user_custom_add_event_action, {
        customevents_names <-
          dplyr::tbl(database(), in_schema("public", "d_customevents")) %>% select("label") %>% collect()
        if (!(input$user_custom_label %in% as.list(customevents_names)$label)) {
          d_custom_events_data <-
            data.frame(
              label = c(input$user_custom_label),
              abbreviation = c(input$user_custom_abbr),
              author = c(input$user_custom_author),
              total_row = c(nrow(file_data()))
            )
          row_added <- dbAppendTable(
            database(),
            Id(schema = "public", table = "d_customevents"),
            as.data.frame(d_custom_events_data)
          )
          new_customevents_id <-
            as.numeric(
              dplyr::tbl(database(), in_schema("public", "d_customevents")) %>% filter(label == !!input$user_custom_label) %>% select(itemid) %>% head(1) %>% collect()
            )
          if (row_added == 1) {
            withProgress(message = 'Persisting data in database',
                         value = 2,
                         max = 5,
                         {
                           custom_events_data <- file_data() %>% select(subject_id,
                                                                        hadm_id,
                                                                        stay_id,
                                                                        charttime,
                                                                        value,
                                                                        valueuom) %>%
                             mutate(itemid = new_customevents_id)
                           row_added <- dbAppendTable(
                             database(),
                             Id(schema = "public", table = "customevents"),
                             as.data.frame(custom_events_data)
                           )
                           setProgress(value = 5)
                           is_previewed(FALSE)
                           invalidator(invalidator() + 1)
                           message_box(
                             "Custom Event successfully persisted",
                             paste0(row_added, " rows has been added for this event"),
                             class = "success",
                             closable = T
                           )
                         })

          } else{
            toast(
              "A error occured when trying to create the custom event in database. Custom event persistence has been cancelled",
              "red"
            )
          }
        } else{
          toast(
            "",
            paste0(
              "A custom event named \"",
              htmlEscape(input$user_custom_label),
              "\" already exist in database. Custom Event name should be unique"
            ),
            "red"
          )
        }
      })

      invalidator <- reactiveVal(0)

      output$custom_event_table <- renderUI({
        invalidator()
        query <- 'SELECT "itemid", "label", "abbreviation", "author", "total_row"
                                    FROM "public"."d_customevents"
                                    ORDER BY "itemid"'
        data_table <- dplyr::tbl(database(), sql(query)) %>% collect()
        tags$table(
          tags$thead(
            tags$th("Event Label"),
            tags$th("Abbreviation"),
            tags$th("Author"),
            tags$th("Event Count"),
            tags$th("Action")
          ),
          tags$tbody(tagList(
            apply(data_table, MARGIN = 1, function(row) {
              observeEvent(input[[paste0("delete-", row['itemid'])]], {
                create_modal(modal(
                  id = ns(paste0("delete-modal-", row['itemid'])),
                  header = h2("Delete a custom event"),
                  content = paste0("Are you sure you want to delete ", row['label'], " ?"),
                  footer = tagList(
                    button(ns(
                      paste0("cancel_delete_", row['itemid'])
                    ), label = "Cancel"),
                    button(
                      ns(paste0("confirm_delete_", row['itemid'])),
                      label = "Confirm delete",
                      class = "red"
                    )
                  )
                ))
                show_modal(ns(paste0("delete-modal-", row['itemid'])))
              })
              observeEvent(input[[paste0("cancel_delete_", row['itemid'])]], {
                hide_modal(ns(paste0("delete-modal-", row['itemid'])))
              })
              observeEvent(input[[paste0("confirm_delete_", row['itemid'])]], {
                runjs(paste0("document.getElementById('",ns(paste0("confirm_delete_", row['itemid'])),"').innerHTML = '<i class=\"notched circle loading icon\"></i>';"))
                query <- "DELETE FROM public.d_customevents WHERE itemid = $1"
                delete <- dbSendQuery(database(), query)
                dbBind(delete, list(row['itemid']))
                dbClearResult(delete)

                hide_modal(ns(paste0("delete-modal-", row['itemid'])))
                invalidator(invalidator() + 1)
              })
              tags$tr(
                tags$td(row['label']),
                tags$td(row['abbreviation']),
                tags$td(row['author']),
                tags$td(row['total_row']),
                tags$td(button(
                  ns(paste0("delete-", row['itemid'])),
                  label = "",
                  icon = icon("trash"),
                  class = "red icon"
                ))
              )
            })
          )),
          class = "ui celled table"
        )
      })

      select_if_value_exist <- function(l,v){
        if(v %in% l){
          v
        } else{
          l[[1]]
        }
      }

      output$user_customfromdb_table_ui <- renderUI({

        sql_get_tables <- "select * from information_schema.TABLES where table_schema NOT IN ('information_schema','pg_catalog')"
        table_list <- dplyr::tbl(database(), sql(sql_get_tables)) %>% select(c("table_schema","table_name")) %>%
          arrange(table_schema, table_name) %>%
          collect() %>%
          rowwise() %>%
          mutate(
            choices = list(tags$span(tagList(tags$b(table_name), tags$span(paste0(" (", table_schema, ")"))))),
            choices_value = paste(table_schema, table_name, sep = "§")
          ) %>%
          ungroup()
        #Replace by a shiny semantic select with custom design <b>table</b> (schema)
        fields(field(tags$label("Choose source table :"),dropdown_input(
          ns("customfromdb_table_filter"),
          table_list$choices,
          table_list$choices_value,
          value = isolate({ifelse(isTruthy(input$customfromdb_table_filter),
                   input$customfromdb_table_filter,
                   " ")})
        )),class="three")
      })

      output$user_customfromdb_fields_ui <- renderUI({
        req(input$customfromdb_table_filter)
        if(isTruthy(input$customfromdb_table_filter) && input$customfromdb_table_filter != " "){
          table_filter <- strsplit(input$customfromdb_table_filter,"§",fixed=T)[[1]]
          query <- "select column_name from information_schema.COLUMNS WHERE table_schema = $1 AND table_name = $2 ORDER BY column_name"
          sql_get_cols <- dbSendQuery(database(), query)

          dbBind(sql_get_cols, list(table_filter[1],table_filter[2]))
          col_list <- dbFetch(sql_get_cols)
          dbClearResult(sql_get_cols)

          subject_id_field <- shiny::selectInput(
            ns("customfromdb_subject_id"),
            "Bind to subject_id",
            col_list$column_name,
            selected = isolate({
              ifelse(isTruthy(input$customfromdb_subject_id),
                     input$customfromdb_subject_id,
                     select_if_value_exist(col_list$column_name,"subject_id"))
            }),
            selectize = F
          )
          hadm_id_field <- shiny::selectInput(
            ns("customfromdb_hadm_id"),
            "Bind to hadm_id",
            col_list$column_name,
            selected = isolate({
              ifelse(isTruthy(input$customfromdb_hadm_id),
                     input$customfromdb_hadm_id,
                     select_if_value_exist(col_list$column_name,"hadm_id"))
            }),
            selectize = F
          )
          if(isTruthy(input$user_customfromdb_check_subject) && input$user_customfromdb_check_subject){
            subject_id_field <- htmltools::tagAppendAttributes(subject_id_field,.cssSelector = "select",disabled="disabled")
          }
          if(isTruthy(input$user_customfromdb_check_hadm) && input$user_customfromdb_check_hadm){
            hadm_id_field <- htmltools::tagAppendAttributes(hadm_id_field,.cssSelector = "select",disabled="disabled")
          }
          tagList(fields(
            field(subject_id_field),
            field(hadm_id_field),
            field(shiny::selectInput(
              ns("customfromdb_stay_id"),
              tagList("Bind to stay_id",tags$i("(optionnal)",class="fw-normal")),
              c(list(" " = " "),col_list$column_name),
              selected = isolate({
                ifelse(isTruthy(input$customfromdb_stay_id),
                       input$customfromdb_stay_id,
                       select_if_value_exist(c(list(" " = " "),col_list$column_name),"stay_id"))
              }),
              selectize = F
            )),
            field(shiny::selectInput(
              ns("customfromdb_charttime"),
              "Bind to charttime :",
              col_list$column_name,
              selected = isolate({
                ifelse(isTruthy(input$customfromdb_charttime),
                       input$customfromdb_charttime,
                       select_if_value_exist(col_list$column_name,"charttime"))
              }),
              selectize = F
            )),
            field(shiny::selectInput(
              ns("customfromdb_value"),
              "Bind to value",
              col_list$column_name,
              selected = isolate({
                ifelse(isTruthy(input$customfromdb_value),
                       input$customfromdb_value,
                       select_if_value_exist(col_list$column_name,"value"))
              }),
              selectize = F
            )),
            field(shiny::selectInput(
              ns("customfromdb_valueuom"),
              tagList("Bind to valueuom",tags$i("(optionnal)",class="fw-normal")),
              c(list(" " = " "),col_list$column_name),
              selected = isolate({
                ifelse(isTruthy(input$customfromdb_valueuom),
                       input$customfromdb_valueuom,
                       select_if_value_exist(c(list(" " = " "),col_list$column_name),"valueuom"))
              }),
              selectize = F
            ))
          ,class="m0 six"),
          fields(field(checkboxInput(ns("user_customfromdb_check_subject"),label="Impute from hadm/stay ID",value = isolate({
            ifelse(isTruthy(input$user_customfromdb_check_subject),
                   input$user_customfromdb_check_subject,
                   FALSE || isTruthy(input$user_customfromdb_check_hadm))
          }))),
                 field(checkboxInput(ns("user_customfromdb_check_hadm"),label="Impute from stay ID",value = isolate({
                   ifelse(isTruthy(input$user_customfromdb_check_hadm),
                          input$user_customfromdb_check_hadm,
                          FALSE)
                 }))),class="six")
          )
        }
      })

      output$user_customfromdb_desc_ui <- renderUI({
        req(input$customfromdb_table_filter)
        tagList(
          field(custom_text_input(ns("user_customfromdb_label"), label = "Insert your event label")),
          field(custom_text_input(ns("user_customfromdb_abbr"), label = "Insert your event abbreviation")),
          field(custom_text_input(ns("user_customfromdb_author"), label = "Insert the author name")),
          button(ns("user_customfromdb_persist_event_action"),label = "Persist event in database")
        )
      })

      output$persist_customfromdb_event <- renderUI({
        req(persistCustomfromdbResult())
        div(persistCustomfromdbResult(), class = "my-10")

      })

      persistCustomfromdbResult <- eventReactive(input$user_customfromdb_persist_event_action, {
        if(isTruthy(input$customfromdb_valueuom) && input$user_customfromdb_label != "" && input$user_customfromdb_author != "" && input$user_customfromdb_abbr != "") {
        customevents_names <-
          dplyr::tbl(database(), in_schema("public", "d_customevents")) %>% select("label") %>% collect()
        if (!(input$user_customfromdb_label %in% as.list(customevents_names)$label)) {
          table_filter <- strsplit(input$customfromdb_table_filter,"§",fixed=T)[[1]]

          query <- "SELECT reltuples::bigint AS estimate FROM pg_class WHERE oid = $1::regclass"
          query <- dbSendQuery(database(), query)

          dbBind(query, list(paste0(table_filter[1],".",table_filter[2])))
          nrow <- dbFetch(query)
          dbClearResult(query)

          d_custom_events_data <-
            data.frame(
              label = c(input$user_customfromdb_label),
              abbreviation = c(input$user_customfromdb_abbr),
              author = c(input$user_customfromdb_author),
              total_row = c(nrow$estimate[1])
            )
          dedicated_db_link <- connect_to_mimic()
          dbBegin(dedicated_db_link)
          row_added <- dbAppendTable(
            dedicated_db_link,
            Id(schema = "public", table = "d_customevents"),
            as.data.frame(d_custom_events_data)
          )
          new_customevents_id <-
            as.numeric(
              dplyr::tbl(dedicated_db_link, in_schema("public", "d_customevents")) %>% filter(label == !!input$user_customfromdb_label) %>% select(itemid) %>% head(1) %>% collect()
            )
          if (row_added == 1) {
            tryCatch({
              withProgress(message = 'Persisting data in database (can take a few minutes)',
                           value = 2,
                           max = 5,
                           {

                             icustays <- dplyr::tbl(dedicated_db_link,in_schema("mimiciv_icu","icustays"))
                             subject_id_col_name <- input$customfromdb_subject_id
                             hadm_id_col_name <- input$customfromdb_hadm_id

                             query <- "select column_name from information_schema.COLUMNS WHERE table_schema = $1 AND table_name = $2 ORDER BY column_name"
                             query <- dbSendQuery(database(), query)
                             dbBind(query, list(table_filter[1],table_filter[2]))
                             col_list <- dbFetch(query)
                             dbClearResult(query)


                             if(input$user_customfromdb_check_subject) {
                               if("stay_id"%in% col_list$column_name){
                                 join_by <- c("stay_id")
                                 subject_id_col_name <- "subject_id"
                                 hadm_id_col_name <- "hadm_id"
                               }
                               if("hadm_id"%in% col_list$column_name){
                                 join_by <- c("hadm_id",join_by)
                               }
                               if("subject_id"%in% col_list$column_name){
                                 join_by <- c("subject_id",join_by)
                               }
                             }
                             else if(input$user_customfromdb_check_hadm) {
                               if("stay_id"%in% col_list$column_name){
                                 join_by <- c("stay_id")
                                 subject_id_col_name <- "subject_id"
                                 hadm_id_col_name <- "hadm_id"
                               }
                               else{
                                 stop("Can't impute with missing stay_id")
                               }
                               if("hadm_id"%in% col_list$column_name){
                                 join_by <- c("hadm_id",join_by)
                               }
                               if("subject_id"%in% col_list$column_name){
                                 join_by <- c("subject_id",join_by)
                               }

                             }
                             custom_events_data <- dplyr::tbl(dedicated_db_link, in_schema(table_filter[1], table_filter[2])) %>%

                               {
                                 if(!!input$user_customfromdb_check_subject){
                                   inner_join(.,icustays,by=join_by)
                                 } else{
                                   .
                                 }
                               } %>%
                               {
                                 if(!!input$customfromdb_stay_id != " "){
                                   mutate(.,stay_id=!!sym(input$customfromdb_stay_id))
                                 }
                                 else{
                                   mutate(.,stay_id=NA)
                                 }
                               } %>%
                               {
                                 if(!!input$customfromdb_valueuom != " "){
                                   mutate(.,valueuom=!!sym(input$customfromdb_valueuom))
                                 }
                                 else{
                                   mutate(.,valueuom=NA)
                                 }
                               } %>%
                               transmute(subject_id=!!sym(subject_id_col_name),
                                         hadm_id=!!sym(hadm_id_col_name),
                                         stay_id,
                                         charttime=!!sym(input$customfromdb_charttime),
                                         value=substr(as.character(!!sym(input$customfromdb_value)),0,20),
                                         valueuom,
                                         itemid = new_customevents_id
                               ) %>% collect()


                             row_added <- dbAppendTable(
                               dedicated_db_link,
                               Id(schema = "public", table = "customevents"),
                               as.data.frame(custom_events_data)
                             )
                             setProgress(value = 5)
                             dbCommit(dedicated_db_link)
                             invalidator(invalidator() + 1)
                             message_box(
                               "Custom Event successfully persisted",
                               paste0(row_added, " rows has been added for this event"),
                               class = "success",
                               closable = T
                             )
                           })
            },
            error = function(error) {
              dbRollback(dedicated_db_link)
              message_box(
                "Error",
                tagList(tags$p("A error occured when trying to create the custom event in database. Custom event persistence has been cancelled."),tags$pre(iconv(error$message, "latin1", "UTF-8"))),
                class = "error",
                closable = T
              )
            })


          } else{
            toast(
              "A error occured when trying to create the custom event in database. Custom event persistence has been cancelled",
              "red"
            )
          }
        } else{
          toast(
            "",
            paste0(
              "A custom event named \"",
              htmlEscape(input$user_customfromdb_label),
              "\" already exist in database. Custom Event name should be unique"
            ),
            "red"
          )
        }
        } else{
          toast(
            "",
              "Please fill every element of the form before persisting",
            "red"
          )
        }
      })

    }

  })
}
