parametersExplorationUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h2("Measured parameters list",tags$small("(without user-added events)")),
    withWaiter(DT::dataTableOutput(ns('params_list')),html=spin_ring(),color = "transparent")
  )
}
