eventSearchbarServer <-
  function(id,
           database = NULL,
           search_label = "Select the parameters you want to put a condition on",
           is_realtime = F,
           trigger_label = "Fetch Cohort") {
    moduleServer(id,
                 function(input, output, session) {
                   ns <- session$ns
                   if (!is.null(database)) {

                     semantic_color_list <-
                       c(
                         "red",
                         "orange",
                         "yellow",
                         "olive",
                         "green",
                         "teal",
                         "blue",
                         "violet",
                         "purple",
                         "pink",
                         "brown",
                         "grey",
                         "black"
                       )



                     # ************************************************************************************#
                     #-------------------------- PARAMETER SELECTION --------------------------------------
                     # ____________________________________________________________________________________#



                     output$add_filter <- renderUI({
                       #search_input <- custom_search_selection_choices(ns("select_filter"),
                       # choices)
                       search_input <- search_selection_api(ns("select_filter"), search_api_url_distinct_events)
                       form(fields(field(
                         tags$label(search_label),
                         search_input
                       )))

                     })

                     filter <- reactiveValues(data = list())

                     unique_label_id <- reactiveVal(100000)



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

                     uiItemFilterGenerator <-
                       function(selected_itemid,
                                selected_item_name,
                                unique_label_id) {
                         param_info <-
                           dplyr::tbl(database(), in_schema("public", "distinct_events")) %>%
                           filter(itemid == selected_itemid) %>%
                           select("param_type", "linksto") %>%
                           collect()
                         if (!is.na(param_info[["param_type"]]) && (param_info[["param_type"]] == "Date and Time" ||
                                                                    param_info[["linksto"]] == "datetimeevents")) {
                           constraint_list <- list("Exist" = "Exist")
                           aggr_func <- list("No aggr fct" = "")
                         } else{
                           constraint_list <- list(
                             "Exist (all)" = "Exist",
                             "= (numerical)" = "=",
                             "<> (numerical)" = "<>",
                             "< (numerical)" = "<",
                             "≤ (numerical)" = "<=",
                             "> (numerical)" = ">",
                             "≥ (numerical)" = ">=",
                             "Is (text)" = "Is",
                             "Is Not (text)" = "Is Not",
                             "Contains (text)" = "Contains",
                             "Not Contains (text)" = "Not Contains",
                             "Is True (bool)" = "Is True",
                             "Is False (bool)" = "Is False"
                           )
                           aggr_func <- list(
                             "No aggr fct" = "",
                             "mean" = "mean",
                             "min" = "min",
                             "max" = "max",
                             "median" = "median",
                             "sum" = "sum"
                           )
                         }
                         uiTargetField <- tagAppendAttributes(
                           shiny::selectInput(
                             ns(
                               paste0(
                                 selected_itemid,
                                 "-",
                                 unique_label_id,
                                 "-constraint_field"
                               )
                             ),
                             tagList(
                               "Select your target field :",
                               icon("inline info circle icon link"),
                               tags$div(tags$div(HTML("<b>value</b> contains the value measured for the concept identified by the itemid.
                                                      If this value is numeric, then <b>valuenum</b> contains the same data in a numeric format.<br>
                                                      If this data is not numeric, <b>valuenum</b> is null. <br>
                                                      In some cases (e.g. scores like Glasgow Coma Scale), <b>valuenum</b> contains the score and <b>value</b> contains the score and text describing the meaning of the score.<br>
                                                      For labs result, please look at the <i class='glasses icon'></i> preview tool to have an overview of the data format<br> Source : <a href='https://mimic.mit.edu/docs/iv/modules/icu/chartevents/'>mimic.mit.edu</a>"),class="content"),class="ui popup right center transition wide hidden")
                             ),
                             allowed_target[param_info[["linksto"]]],
                             selectize = F
                           ),
                           class = paste0(
                             "ui sixteen wide field ",
                             ns("constraint-selector"),
                             " constraint-field"
                           )
                         )

                         uiConstraintType <- tagAppendAttributes(
                           shiny::selectInput(
                             ns(
                               paste0(
                                 selected_itemid,
                                 "-",
                                 unique_label_id,
                                 "-constraint_type"
                               )
                             ),
                             "Select your constraint type :",
                             constraint_list,

                             selectize = F
                           ),
                           class = paste0(
                             "ui sixteen wide field ",
                             ns("constraint-selector"),
                             " constraint-type"
                           )
                         )
                         uiConstraintAggr <- tagAppendAttributes(
                           shiny::selectInput(
                             ns(
                               paste0(
                                 selected_itemid,
                                 "-",
                                 unique_label_id,
                                 "-constraint_type"
                               )
                             ),
                             "Select your aggregation function :",
                             aggr_func,

                             selectize = F
                           ),
                           class = paste0(
                             "ui sixteen wide field ",
                             ns("constraint-selector"),
                             " constraint-aggr"
                           )
                         )
                         uiConstraintValue <-
                           tagAppendAttributes(div(
                             tagAppendAttributes(
                               tags$label(tagList(
                                 tags$span(),
                                 tagAppendAttributes(icon("right glasses"), style = "cursor:pointer;")
                               ), style = "text-align:right;max-height:2.7em;overflow-y:scroll;"),
                               class = ns("constraint-helper")
                             ),
                             tagAppendAttributes(
                               text_input(ns(
                                 paste0(
                                   selected_itemid,
                                   "-",
                                   unique_label_id,
                                   "-constraint_value"
                                 )
                               ),
                               label = NULL,
                               value = ""),
                               class = ns("constraint-selector")
                             )
                           ),
                           class = paste0("ui field"))

                         checkboxId <-
                           ns(
                             paste0(
                               selected_itemid,
                               "-",
                               unique_label_id,
                               "-constraint-is-time-restricted"
                             )
                           )
                         uiConstraintTimeCheckbox <-
                           custom_checkbox_input(
                             checkboxId,
                             type = paste(
                               "slider",
                               "constraint-time-enabled",
                               ns("constraint-selector"),
                               "my-10"
                             ),
                             label = "Use a time windows",
                             is_marked = ifelse(isTruthy(input[[checkboxId]]),
                                                TRUE,
                                                FALSE)
                           )
                         inputTimeMinId <-
                           ns(paste0(
                             selected_itemid,
                             "-",
                             unique_label_id,
                             "-constraint-time-min"
                           ))
                         inputTimeMaxId <-
                           ns(paste0(
                             selected_itemid,
                             "-",
                             unique_label_id,
                             "-constraint-time-max"
                           ))
                         minMaxTimeField <- fields(field(
                           labeled_numeric_input(
                             inputTimeMinId,
                             label = "Min limit (in hours)",
                             value = isolate({
                               ifelse(isTruthy(input[[inputTimeMinId]]),
                                      input[[inputTimeMinId]],
                                      0)
                             }),
                             class = paste("constraint-time-min", ns("constraint-selector")),
                             min = -24,
                             max = 9000,
                             type = "number"
                           )
                         ),
                         field(
                           labeled_numeric_input(
                             inputTimeMaxId,
                             label = "Max limit (in hours)",
                             value = isolate({
                               ifelse(isTruthy(input[[inputTimeMaxId]]),
                                      input[[inputTimeMaxId]],
                                      24)
                             }),
                             class = paste("constraint-time-max", ns("constraint-selector")),
                             min = -24,
                             max = 9000,
                             type = "number"
                           )
                         ))




                         tagList(
                           div(paste0(
                             selected_item_name, " configuration"
                           ), class = "ui header"),
                           fields(uiTargetField,
                                  uiConstraintType,
                                  uiConstraintAggr),
                           uiConstraintValue,
                           segment(
                             uiConstraintTimeCheckbox,
                             htmltools::tagAppendAttributes(minMaxTimeField, style =
                                                              "display:none;")
                           )
                         )
                       }

                     if(!is.null(session$userData[[ns("observe_filter")]])){
                       session$userData[[ns("observe_filter")]]$destroy()
                     }
                     session$userData[[ns("observe_filter")]] <- observeEvent(input$select_filter, {
                       if (input$select_filter != "") {
                         e_color = semantic_color_list[(as.integer(event_colors[input$select_filter]) %%
                                                          13) + 1]
                         text = distinct_events %>% filter(value==!!input$select_filter) %>% select(name) %>% as.character()

                         #Do not ns in shinyjs::html
                         shinyjs::html("filter-container",  as.character(
                           div(
                             div(
                               tagList(
                                 icon("caret square down outline"),
                                 span(text),
                                 div(class = "ui label")
                               ),
                               icon(class = "linkstolabel close"),
                               value = paste0(input$select_filter),
                               class = paste0("ui label filter ", e_color),
                               id = ns(paste0("label-", as.character(unique_label_id())))
                             ),
                             tagAppendAttributes(
                               div(
                                 uiItemFilterGenerator(input$select_filter, text, unique_label_id()),
                                 class = "ui popup",
                                 style = "max-width:450px;width:450px;z-index:9999999999;position:static;"
                               ),
                               linkedlabel = as.character(unique_label_id())
                             ),
                             contenteditable = "false",
                             class = "label-popup"
                           )
                         ),
                         add = T)

                         runjs(sprintf(
                           "$('.ui.dropdown.%s').dropdown('clear');",
                           ns("select_filter")
                         ))
                         runjs(
                           "$('.ui.label.filter').popup({on:'click',position:'bottom left',movePopup: false});"
                         )
                         runjs(paste0(
                           '$( "#',
                           ns("filter-container"),
                           '" ).trigger( "change" );'
                         ))
                         unique_label_id(unique_label_id() + 1)
                         runjs("$('.inline.icon')
                            .popup({
                              inline: true,
                              on: 'click'
                            })
                          ;")

                       }
                     })


                     output$filter_render <- renderUI({
                       form(field(
                         div(
                           id = ns("filter-container"),
                           contenteditable = "true",
                           class = "ui selection dropdown"
                         ),
                         tags$small(
                           "Default operator between two condition is OR, you must explicit AND condition",
                           style = "font-weight:bold;"
                         )
                       ))
                     })

                     # ************************************************************************************#
                     #-------------------------- PARAMETER PREVIEW IN POPUP --------------------------------
                     # ____________________________________________________________________________________#
                     numeric_preview <-
                       function(database,
                                table_schema,
                                linksto,
                                selected_itemid,
                                selected_field,
                                need_cast = F) {
                         if (need_cast) {
                           base_query <- paste0(
                             "SELECT CASE WHEN ",
                             selected_field,
                             "~E'^[+-]?([0-9]*[.])?[0-9]+$' THEN ",
                             selected_field,
                             "::numeric ELSE NULL end AS ",
                             selected_field,

                             "
                          FROM ",
                             table_schema,
                             ".",
                             linksto,
                             ifelse(linksto=="prescriptions"," JOIN public.d_prescriptions USING(drug) ",""),
                             " WHERE itemid = ",
                             selected_itemid
                           )


                         } else{
                           table <-
                             dplyr::tbl(database(), in_schema(table_schema, linksto))
                           base_query <-
                             dbplyr::sql_render(table %>%
                                                  {
                                                    if(linksto == "prescriptions"){
                                                      inner_join(.,dplyr::tbl(database(),
                                                                              in_schema("public", "d_prescriptions")),by="drug")
                                                    }else{
                                                      .
                                                    }
                                                  } %>%
                                                  filter(itemid == selected_itemid) %>%
                                                  select(!!eval(selected_field)))
                         }

                         sql_query <- paste0(
                           'SELECT
                                      MIN(',
                           selected_field,
                           ') AS "min",
                                      PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ',
                           selected_field,
                           ') AS "median",
                                      AVG(',
                           selected_field,
                           ') AS "mean",
                                      PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY ',
                           selected_field,
                           ') AS "q25",
                                      PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY ',
                           selected_field,
                           ') AS "q75",
                                      MAX(',
                           selected_field,
                           ') AS "max"
                                    FROM (SELECT cast(COALESCE(',
                           selected_field,
                           ') as numeric) AS "',
                           selected_field,
                           '" FROM (',
                           base_query,
                           ') "q02") "q01"'
                         )
                         print(sql_query)
                         request <-
                           dplyr::tbl(database(), dplyr::sql(sql_query)) %>% collect()

                         value <-
                           paste0(
                             "min:",
                             request[["min"]],
                             ", Q1:",
                             request[["q25"]],
                             ", median:",
                             request[["median"]],
                             ", Q3:",
                             request[["q75"]],
                             ", max:",
                             request[["max"]]
                           )
                       }

                     observe({
                       if (length(input$constraint_helper_request) > 0) {
                         # Check if cache folder exist
                         ifelse(!dir.exists(paste0(
                           CONFIG$CACHE_DIR, "cache/cohort_creation/"
                         )),
                         dir.create(
                           paste0(CONFIG$CACHE_DIR, "cache/cohort_creation/"),
                           recursive = TRUE
                         ),
                         FALSE)

                         parameters <-
                           unlist(strsplit(input$constraint_helper_request, split = ","))

                         labelId <- parameters[1]
                         selected_itemid <- parameters[2]
                         selected_field <- parameters[3]
                         value <- NULL

                         param_info <-
                           dplyr::tbl(database(), in_schema("public", "distinct_events")) %>%
                           filter(itemid == selected_itemid) %>%
                           select("param_type", "linksto") %>%
                           collect()


                         if (file.exists(paste0(
                           CONFIG$CACHE_DIR,
                           "cache/cohort_creation/",
                           param_info[["linksto"]],
                           ".Rda"
                         ))) {
                           cachedTibble <-
                             as_tibble(readRDS(
                               paste0(
                                 CONFIG$CACHE_DIR,
                                 "cache/cohort_creation/",
                                 param_info[["linksto"]],
                                 ".Rda"
                               )
                             ))
                           data <-
                             cachedTibble %>% filter(itemid == !!selected_itemid &
                                                       field == !!selected_field)
                           if (nrow(data) > 0) {
                             value <- data[['value']]
                           }
                         } else{
                           cachedTibble <- NULL
                         }


                         if (is.null(value)) {
                           #For large/complex requesting, use a dedicated db connection to not freeze app
                           dedicated_db_link <- connect_to_mimic()
                           table_schema <-
                             get_table_schema(param_info[["linksto"]])
                           table <-
                             dplyr::tbl(dedicated_db_link,
                                        in_schema(table_schema, param_info[["linksto"]]))

                           if (!is.na(param_info[["param_type"]]) && param_info[["param_type"]] == "Text") {
                             request <- table %>%
                               filter(itemid == selected_itemid) %>%
                               select(!!eval(selected_field)) %>%
                               distinct() %>% collect()
                             value <-
                               paste(request[[selected_field]], collapse = ", ")
                           } else if (!is.na(param_info[["param_type"]]) && param_info[["param_type"]] == "Date and Time") {
                             value <-  "Not implemented yet : DateTime field"
                           } else if (!is.na(param_info[["param_type"]]) && param_info[["param_type"]] == "Checkbox") {
                             value <- "1,0"
                           } else if (!is.na(param_info[["param_type"]]) && param_info[["param_type"]] == "Numeric") {
                             # Find min-max
                             value <-
                               numeric_preview(
                                 dedicated_db_link,
                                 table_schema,
                                 param_info[["linksto"]],
                                 selected_itemid,
                                 selected_field
                               )

                           } else{
                             data <- table %>%
                               {
                                 if(param_info[["linksto"]] == "prescriptions"){
                                   inner_join(.,dplyr::tbl(dedicated_db_link,
                                                           in_schema("public", "d_prescriptions")),by="drug")
                                 }else{
                                   .
                                 }
                               } %>%
                               filter(itemid == selected_itemid) %>%
                               select(!!eval(selected_field)) %>%
                               head(100) %>% collect()
                             column_type <-
                               get_column_type(selected_field, table_schema, param_info[["linksto"]])
                             if (!(column_type %in% list("character varying","text"))) {
                               value <-
                                 numeric_preview(
                                   dedicated_db_link,
                                   table_schema,
                                   param_info[["linksto"]],
                                   selected_itemid,
                                   selected_field,
                                   need_cast = F
                                 )
                             }
                             else{
                               if (sum(check.numeric(data[[selected_field]])) > 50) {
                                 #More than 50% of the 100 first value are numeric, apply a numeric helper
                                 # Find min-max
                                 value <-
                                   numeric_preview(
                                     dedicated_db_link,
                                     table_schema,
                                     param_info[["linksto"]],
                                     selected_itemid,
                                     selected_field,
                                     need_cast = T
                                   )
                               }
                               else{
                                 request <- table %>%
                                   {
                                     if(param_info[["linksto"]] == "prescriptions"){
                                       inner_join(.,dplyr::tbl(dedicated_db_link,
                                                               in_schema("public", "d_prescriptions")),by="drug")
                                     }else{
                                       .
                                     }
                                   } %>%
                                   filter(itemid == selected_itemid) %>%
                                   select(!!eval(selected_field)) %>%
                                   distinct() %>% collect()
                                 value <-
                                   paste(request[[selected_field]], collapse = ", ")
                               }
                             }

                           }
                           updatedCache <-
                             bind_rows(
                               cachedTibble,
                               tibble(
                                 itemid = selected_itemid,
                                 field = selected_field,
                                 value = value
                               )
                             )
                           saveRDS(
                             updatedCache,
                             file = paste0(
                               CONFIG$CACHE_DIR,
                               "cache/cohort_creation/",
                               param_info[["linksto"]],
                               ".Rda"
                             )
                           )
                           dbDisconnect(dedicated_db_link)
                         }

                         session$sendCustomMessage(ns("setConstraintHelper"), paste0(labelId, "&", value))
                       }
                     })

                     # ************************************************************************************#
                     #------------------------------- ICD CODE RESTRICTION ---------------------------------
                     # ____________________________________________________________________________________#


                     output$icd_code_restriction <- renderUI({
                       form(fields(field(
                         tags$label("Required hadm affected ICD code to be included in the cohort (child ICD are automatically included)"),
                         search_selection_api(ns("icd_to_keep"), search_api_url_icd_data, multiple = TRUE),
                         class="six wide"
                       ),field(
                         selectInput(ns("icd_to_keep_condition"),"Condition to apply to included ICD code list",c("OR","AND")),
                         class="two wide"
                       ),
                       field(
                         tags$label("Required hadm affected ICD to be excluded of the cohort (child ICD are automatically excluded)"),
                         search_selection_api(ns("icd_to_deny"), search_api_url_icd_data, multiple = TRUE),
                         class="six wide"
                       ),field(
                         selectInput(ns("icd_to_deny_condition"),"Condition to apply to excluded ICD code list",c("OR","AND")),
                         class="two wide"
                       )))
                     })




                     # ************************************************************************************#
                     #------------------------------- FETCH COHORT ACTION ----------------------------------
                     # ____________________________________________________________________________________#



                     output$fetch_action <- renderUI({
                       if (unique_label_id() > 100000 & !is_realtime) {
                         #at least one condition
                         tagList(tagAppendAttributes(field(
                           button(ns("fetch-button"), label = trigger_label)
                         ), class = "my-10"))
                       }

                     })


                     condition_to_filter <-
                       list("AND" = "&", "OR" = "|")

                     enclose_to_filter <-
                       list("open" = " ( ", "close" = " ) ")

                     condition_object <- reactive({

                       if (length(input$filter_tojson) != 0) {
                         result_object <-
                           fromJSON(input$filter_tojson, simplifyVector = FALSE)
                         if (result_object$message != "OK") {
                           shiny.semantic::toast(
                             message= paste0(
                               "<b>Can't set an event condition </b><br>",
                               HTML(result_object$message)
                             ),
                             class="warning",
                             duration=10
                           )
                         } else{
                           if(length(result_object$result)>0){
                             conditionDF <- result_object$result
                             condition_string <- ""
                             condition_id <- 1
                             constraint_list <- list()
                             # Used to complete implicit condition separator (OR by default)
                             can_expect_condition_separator = F
                             for (rowidx in 1:length(conditionDF)) {
                               row <- conditionDF[[rowidx]]
                               if (row$type == "filter") {
                                 if (can_expect_condition_separator) {
                                   condition <- condition_to_filter[["OR"]]
                                   condition_string <-
                                     paste0(condition_string, condition)
                                 }
                                 can_expect_condition_separator <- T
                                 linksto <-
                                   (
                                     dplyr::tbl(
                                       database(),
                                       in_schema("public", "distinct_events")
                                     ) %>% filter(itemid == !!row$itemid) %>% select("linksto") %>% collect()
                                   )[[1]]
                                 constraint_list[[as.character(condition_id)]] <-
                                   list()
                                 constraint_list[[as.character(condition_id)]]$constraint <-
                                   list(
                                     itemid = row$itemid,
                                     constraint = row$constraint,
                                     aggr = row$aggr,
                                     field = row$field,
                                     value = row$value,
                                     time_constraint = list(
                                       "is_time_constrained" = row$is_time_constrained,
                                       "time_min" = row$time_min,
                                       "time_max" = row$time_max
                                     )
                                   )

                                 constraint_list[[as.character(condition_id)]]$linksto <-
                                   linksto
                                 condition <-
                                   paste0("condition_", condition_id)
                                 condition_id <- condition_id + 1
                               }
                               else if (row$type == "condition") {
                                 condition <- condition_to_filter[row$element]
                                 can_expect_condition_separator <- F
                               }
                               else if (row$type == "enclose") {
                                 if (can_expect_condition_separator && row$element == "open") {
                                   condition <- condition_to_filter["OR"]
                                   condition_string <-
                                     paste0(condition_string, condition)
                                 }
                                 if (row$element == "close") {
                                   can_expect_condition_separator <- T
                                 } else{
                                   can_expect_condition_separator <- F
                                 }
                                 condition <-
                                   enclose_to_filter[row$element]
                               }
                               condition_string <-
                                 paste0(condition_string, condition)
                             }
                             list("condition_string" = condition_string,
                                  "constraint_list" = constraint_list,
                                  "icd_to_allow" = isolate(input$icd_to_keep),
                                  "icd_to_deny" = isolate(input$icd_to_deny),
                                  "allow_condition" = isolate(input$icd_to_keep_condition),
                                  "deny_condition" = isolate(input$icd_to_deny_condition))
                           } else if(length(input$icd_to_keep)>0 || length(input$icd_to_deny)>0){
                             list("condition_string" = "",
                                  "constraint_list" = list(),
                                  "icd_to_allow" = isolate(input$icd_to_keep),
                                  "icd_to_deny" = isolate(input$icd_to_deny),
                                  "allow_condition" = isolate(input$icd_to_keep_condition),
                                  "deny_condition" = isolate(input$icd_to_deny_condition))
                           } else{
                             shiny.semantic::toast(
                               message= paste0(
                                 "<b>Stratifying parameters are empty but stratification is enabled</b><br>
                                 Displaying graph without stratification. If you are trying to create a cohort, you should add a parameter in the condition bar"
                               ),
                               class="warning",
                               duration=10
                             )
                             NULL
                           }

                         }

                       }
                     })


                     # ************************************************************************************#
                     #-------------------------------- INJECTED JAVASCRIPT ---------------------------------
                     # ____________________________________________________________________________________#

                     # If numerical condition & "value" field selected, switch for valuenum if exist

                     shinyjs::runjs(
                       paste0(
                         "$('body').on('change', \"select[id^='",
                         substr(ns(""), 1, nchar(ns("")) - 1),
                         "'][id$='-constraint_type']\", function() {
                      let constraint_type = $(this).val();
                      if(['=','>','<','<>','<=','>='].includes(constraint_type)){
                        let constraint_field_select = $(this).parent().parent().parent().find(\"select[id$='-constraint_field']\");
                        let choices = $.map(constraint_field_select.find('option') ,function(option) {
                          return option.value;
                        });

                        if(choices.includes(\"valuenum\")){
                          constraint_field_select.find('option').removeAttr('selected')
                          constraint_field_select.val('valuenum').change()
                        }
                      }
                    });"
                       )
                     )

                     # Display and hide time constraint when clicking on slider
                     shinyjs::runjs(
                       paste0(
                         "$('body').on('click', '.constraint-time-enabled > input', function() {
                            if($(this).prop('checked')){
                              $(this).parent().parent().find('.fields').show();
                            } else{
                              $(this).parent().parent().find('.fields').hide();
                            }

                         });"
                       )
                     )


                     # JS to display a small tooltip on label to preview set condition
                     # + Hard write in dom user selected value in order to prevent reset when creating new popup
                     shinyjs::runjs(
                       paste0(
                         "$('body').on('change', '.",
                         ns("constraint-selector"),
                         "', function() {
                       eventTarget = $(this);
                       let labelId = eventTarget.closest('.ui.popup').attr('linkedlabel');
                       $('#",ns("label"),"-'+labelId+' > .ui.label').html('');
                       eventTarget.parent().parent().find('.",
                         ns("constraint-selector"),
                         "').each(function (index){
                       if ($(this).is('input')) {
                         $(this).attr('value',$(this).val())
                       }
                       else if($(this).hasClass('constraint-time-enabled')){
                          if($(this).find('input').prop('checked')){
                              $(this).attr('checked');
                          } else{
                              $(this).removeAttr('checked');
                          }
                       }
                       else{
                       $(this).find('select option').removeAttr('selected');
                       $(this).find('select option[value=\"' + $(this).find('select').val()+ '\"]').attr('selected','selected');
                       }
                       })
                       popup = eventTarget.closest('.ui.popup');
                       constraintField = popup.find('.constraint-field > div > select').val();
                       constraintType = popup.find('.constraint-type > div > select').val();
                       constraintAggr = popup.find('.constraint-aggr > div > select').val();
                       constraintTime = '';
                       if(popup.find('.constraint-time-enabled > input').prop('checked')){
                          constraintTimeMin = popup.find('.constraint-time-min').val();
                          constraintTimeMax = popup.find('.constraint-time-max').val();
                          constraintTime = ' (' + constraintTimeMin + 'h-' + constraintTimeMax + 'h)';
                       }
                       constraintValue = popup.find('input').val();
                       if(constraintAggr!=''){
                        constraintField = constraintAggr + '(' + constraintField + ')';
                       }
                        $('#",ns("label"),"-'+labelId+' > .ui.label').html(constraintField + ' ' + constraintType + ' ' + constraintValue + constraintTime);
                       });
                       $('body').on('click','.linkstolabel.close',function(e) {
                          $(e.target).parent().parent().remove()
                        });
                        "
                       )
                     )

                     # Handle fetch-button click on client side and use htmlToCondition to convert html to a json string describing user filter
                     {
                       if (is_realtime) {
                         shinyjs::runjs(
                           paste0(
                             "$('body').on('keyup paste input change focusin focusout', '#",
                             ns("filter-container"),
                             ",#",ns("icd_to_keep"),",#",ns("icd_to_deny"),",#",ns("icd_to_keep_condition"),",#",ns("icd_to_keep_condition"),"', function() {
                       result = htmlToCondition('",
                             ns("filter-container"),
                             "');
                       console.log(result);
                              //Set up data to shiny here
                       Shiny.setInputValue('",
                             ns('filter_tojson'),
                             "', JSON.stringify(result));});"

                           )
                         )
                       } else{
                         shinyjs::runjs(
                           paste0(
                             "$('body').on('click', '#",
                             ns("fetch-button"),
                             "', function() {
                       result = htmlToCondition('",
                             ns("filter-container"),
                             "');
                       if(result !== null){
                       console.log(result)
                                                     //Set up data to shiny here
                       Shiny.setInputValue('",
                             ns('filter_tojson'),
                             "', JSON.stringify(result));
                       }
});"
                           )
                         )
                       }
                     }




                     # Client-side function converting HTML DOM inside div with targetFilterContainerID in a javascript condition array
                     # Check also parenthesis parsing and unrecognised keyword in div
                     shinyjs::runjs(
                       paste0(
                         "window.htmlToCondition = function (targetFilterContainerID) {
                      container = $('#' + targetFilterContainerID);
                      contents = container.contents();

                      chainCondition = [];
                      isInconsistent = false;
                      errorMessage = '';
                      zClosing = 0;// +1 for open bracket, -1 for close bracket
                      contents.each(function (index) {
                        if ($(this)[0].nodeValue === null) {
                          itemid = $(this).find('.ui.label.filter').attr('value');
                          constraintField = $(this).find('.constraint-field > div > select').val();
                          constraintType = $(this).find('.constraint-type > div > select').val();
                          constraintAggr = $(this).find('.constraint-aggr > div > select').val();
                          constraintValue = $(this).find('input').val();
                          isTimeConstrained = $(this).find('.constraint-time-enabled > input').prop('checked')
                          constraintTimeMin = $(this).find('.constraint-time-min').val();
                          constraintTimeMax = $(this).find('.constraint-time-max').val();
                          if(['Is','Is Not','Contains','Not Contains','=','<>','>','<','<=','>='].includes(constraintType) && constraintValue.length == 0){
                            errorMessage += 'Constraint is inconsistent (using a with param. constraint without giving any value).<br>';
                            isInconsistent = true;
                            return null;
                          }
                          chainCondition.push({
                            type: 'filter',
                            'itemid': itemid,
                            'field': constraintField,
                            'constraint': constraintType,
                            'aggr': constraintAggr,
                            'value': constraintValue,
                            'is_time_constrained': isTimeConstrained,
                            'time_min': constraintTimeMin,
                            'time_max': constraintTimeMax
                          });
                        } else {
                          //is text
                          text = $(this)[0].nodeValue;
                          idx = 0
                          while (idx < text.length) {
                            if (text[idx] == '(') {
                              zClosing += 1
                              chainCondition.push({ type: 'enclose', element: 'open' });
                            } else if (text[idx] == ')') {
                              zClosing -= 1
                              chainCondition.push({ type: 'enclose', element: 'close' });
                            } else if (text[idx] == 'A') {
                              if (idx + 2 < text.length) {
                                if (text.slice(idx, idx + 3) == 'AND') {
                                  chainCondition.push({ type: 'condition', element: 'AND' });
                                  idx += 2
                                } else {
                                  isInconsistent = true;
                                  errorMessage += 'Unexpected ' + text.slice(idx, idx + 3) + ' in filter parsing.<br>';
                                }

                              }
                            } else if (text[idx] == 'O') {
                              if (idx + 1 < text.length) {
                                if (text.slice(idx, idx + 2) == 'OR') {
                                  chainCondition.push({ type: 'condition', element: 'OR' });
                                  idx += 1
                                } else {
                                  isInconsistent = true;
                                  errorMessage += 'Unexpected ' + text.slice(idx, idx + 2) + ' in filter parsing.<br>';
                                }
                              } else {
                                if (text[idx] != ' ') {
                                  isInconsistent = true;
                                  errorMessage += 'Unexpected ' + text[idx] + ' in filter parsing.<br>';
                                }
                              }

                            }
                            idx += 1
                          }
                        }
                      })
                      if (zClosing == 0) {
                        if(!isInconsistent){
                          return { message: 'OK', result: chainCondition, request_time: Date.now() };
                        } else{
                          return { message: errorMessage, result: null };
                        }
                      } else {
                        return { message: 'Missing opening or closing parenthesis in filter parsing', result: null };
                      }
                      return { message: 'An unexpected error occured', result: null };
                    }"
                       )
                     )
                     # Handle client side preview of the parameters in popup
                     shinyjs::runjs(
                       paste0(
                         "
                        $('body').on('click','.",
                         ns("constraint-helper"),
                         "',function(e) {
                          if( $(e.target).hasClass('glasses')){
                            $(e.target).toggleClass('notched circle loading');
                            $(e.target).toggleClass('glasses');
                            let labelId = $(e.target).closest('.ui.popup').attr('linkedlabel');
                            let itemId = $('#",ns("label"),"-'+labelId+'').attr('value');
                            let constraintField = $('#",ns("label"),"-'+labelId+' + .ui.popup > .fields > .constraint-field > div > select').val();
                            Shiny.setInputValue('",
                         ns('constraint_helper_request'),
                         "', labelId + ',' + itemId + ',' + constraintField);
                          }

                        });
                        Shiny.addCustomMessageHandler('",ns("setConstraintHelper"),"', function(raw_data) {
                          data = raw_data.split('&');
                          console.log('",ns("label"),"-'+ data[0] +' + .ui.popup > .ui.field > label')
                          targetedLabel = $('#",ns("label"),"-'+ data[0] +' + .ui.popup > .ui.field > label');
                          let spanContainer = targetedLabel.find('span')
                          elementList = data[1].split(',');
                          for(el of elementList){
                            let dom = $('<span class=\"constraint-pickable\"></span>').text(el+',');
                            spanContainer.append(dom);
                          }
                          targetedLabel.find('i').toggleClass('notched circle loading check');
                          targetedLabel.find('i').css('cursor','default');
                        });
                        $('body').on('click','.constraint-pickable',function(e) {
                          clickedText = $(e.target).text().slice(0, -1);
                          textToCopy = clickedText.split(':').pop()
                          $(e.target).closest('.ui.popup').find('input').val(textToCopy);
                          $(e.target).closest('.ui.popup').find('input').trigger(jQuery.Event('change'))
                        });
                        "
                       )
                     )
                     # Reset previexw on targeted column change
                     shinyjs::runjs(
                       paste0(
                         "$('body').on('change', \"select[id^='",
                         substr(ns(""), 1, nchar(ns("")) - 1),
                         "'][id$='-constraint_field']\", function(e) {
                         targetedLabel = $(e.target).closest('.ui.popup').find('.ui.field > label');
                         targetedLabel.find('span').html('');
                         targetedLabel.find('i').removeClass('check');
                         targetedLabel.find('i').css('cursor','pointer');
                         targetedLabel.find('i').addClass('glasses');

                    });"
                       )
                     )
                     return(condition_object)
                   }
                 })
  }
