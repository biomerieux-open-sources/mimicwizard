eventSearchbarUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("add_filter")),
    uiOutput(ns("filter_render")),
    uiOutput(ns("icd_code_restriction")),
    uiOutput(ns("fetch_action"))
  )
}
