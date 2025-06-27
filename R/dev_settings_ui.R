devSettingsUI <- function(id) {
  ns <- NS(id)
  tagList(div(style = "display:flex; flex-direction:column;width:100%",
              h3("Developer settings"),
              segment(tagList(
                tags$h4(tagList(
                  icon(class = "grey file medical"),
                  tags$div("Add custom events to the database", class = "content")
                ), class = "ui header"),
                form(
                  tags$p(
                    "Your csv file should have the following column ",tags$code("subject_id,hadm_id,stay_id,charttime,value,valueuom")," (separator : ,)"
                  ),
                  file_input(ns("user_custom_file"), label = "Insert your csv file here"),
                  field(custom_text_input(ns("user_custom_label"), label = "Insert your event label")),
                  field(custom_text_input(ns("user_custom_abbr"), label = "Insert your event abbreviation")),
                  field(custom_text_input(ns("user_custom_author"), label = "Insert the author name")),
                  button(ns("user_custom_preview_event_action"),label = "Preview event in database"),
                ),
                uiOutput(ns("persist_custom_event")),
                uiOutput(ns("preview_custom_event")),

              )),
              segment(tagList(
                tags$h4(tagList(
                  icon(class = "grey file prescription"),
                  tags$div("Manage custom events to the database", class = "content")
                ), class = "ui header"),
                tags$p("Consult or delete custom event added in the database"),
                uiOutput(ns("custom_event_table"))
              )),
              segment(tagList(
                tags$h4(tagList(
                  icon(class = "grey keyboard"),
                  tags$div("Create custom events from database", class = "content")
                ), class = "ui header"),
                form(
                  tags$p(
                    "You can create subevents using all the tables populated in the database. This form allows you to make use of derived table and others supplementary data available on Github (MIT-LCP/mimic-code) or physionet.org"
                  ),
                  uiOutput(ns("user_customfromdb_table_ui")),
                  uiOutput(ns("user_customfromdb_fields_ui")),
                  uiOutput(ns("user_customfromdb_desc_ui")),
                ),
                uiOutput(ns("persist_customfromdb_event")),
                uiOutput(ns("preview_customfromdb_event")),

              ))
              ))
}
