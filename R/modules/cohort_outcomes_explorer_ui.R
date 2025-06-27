cohortOutcomesExplorerUI <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(uiOutput(ns("outcomes_result"))),
    uiOutput(ns("stratification_manager"))
  )
}
