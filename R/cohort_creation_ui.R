cohortCreationUI <- function(id) {
  ns <- NS(id)
  div(style="display:flex; flex-direction:column;width:100%",
    tags$h2("Cohort Creation"),
    tags$p("Cohort creation is useful to generate cohort based on event combination. Cohort creation only create ICU cohort, time-related event are based on ICU in-time. Events can be used up to 24 hours before admission to intensive care unit. "),
    eventSearchbarUI(ns("event_searchbar")),
    withWaiter(uiOutput(ns("fetched_details")),html=spin_ring(),color = "transparent"),
    uiOutput(ns("persist_action")),
    uiOutput(ns("result_persist"))
  )
}
