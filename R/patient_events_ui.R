patientExplorerUI <- function(id) {
  ns <- NS(id)
  div(
    style = "display:flex; flex-direction:column;width:100%",
    uiOutput(ns("select_uid")),
    tags$div("OR",class="ui horizontal divider"),
    uiOutput(ns("select_patient")),
    uiOutput(ns("select_hadm")),
    uiOutput(ns("select_stay")),
    uiOutput(ns("summary")),
    uiOutput(ns("hadm_preview")),
    uiOutput(ns("llm_interactive")),
    div(tagList(
      div(
        div(segment(
          div("Timeline item details", class = "ui header"),
          withWaiter(uiOutput(ns("timevis_details_ui")),html=spin_ring(),color = "transparent")
        ), class = "right-side-div timevis-details"),
        class = "ui segment",
        div(
          div(class = "ui header", "Hospitalisation Timeline"),
          uiOutput(ns("timeline_parameter_selection")),
          uiOutput(ns("timeline_parameter_display")),
          htmlOutput(ns("timeline"))
        ),
        style = "overflow-y:scroll;overflow-x:hidden;"
      ),
      div(class = "ui segment", div(
        div(class = "ui header", "Parameter Tracking"), uiOutput(ns("cards_parameter_selection"))
      ),
      htmlOutput(ns("cards"))),
      verbatimTextOutput(ns("patient_to_text")))
    ,class="my-10",id = ns("patient-explorer-container"))

  )
}
