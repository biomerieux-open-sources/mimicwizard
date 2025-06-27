clinicalDataDescUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("clin_table")))
}
