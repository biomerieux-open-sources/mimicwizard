cohortCreationUI <- function(id) {
  ns <- NS(id)
  div(style="display:flex; flex-direction:column;width:100%",
    tags$h2("Cohort Creation"),
    eventSearchbarUI(ns("event_searchbar")),
    withWaiter(uiOutput(ns("fetched_details")),html=spin_ring(),color = "transparent"),
    uiOutput(ns("persist_action")),
    uiOutput(ns("result_persist"))
  )
}
