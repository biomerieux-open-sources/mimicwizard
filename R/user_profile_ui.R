userProfileUI <- function(id) {
  ns <- NS(id)
  tagList(div(style = "display:flex; flex-direction:column;width:100%",
              h3("Manage profile"),
              segment(tagList(
                tags$h4(tagList(
                  icon(class = "grey user plus"),
                  tags$div("Create a profile", class = "content")
                ), class = "ui header"),
                form(
                  field(custom_text_input(ns("add_user_name"), label = "Insert your profile name")),
                  field(custom_text_input(ns("add_user_desc"), label = "Insert your profile description")),
                  uiOutput(ns("add_user_parameter_ui")),
                  uiOutput(ns("add_user_cohort_ui")),
                  div(button(ns("add_user_action"),label = "Add profile to database",class="teal"),class="my-10")
                ),
                uiOutput(ns("add_user_result")),

              )),
              segment(tagList(
                tags$h4(tagList(
                  icon(class = "grey users"),
                  tags$div("Manage profile in database", class = "content")
                ), class = "ui header"),
                uiOutput(ns("users_table"))
              ))
  ))
}
