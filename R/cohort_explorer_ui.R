cohortExplorerUI <- function(id) {
  ns <- NS(id)
  tagList(htmltools::tagAppendAttributes(menu(
    class = "yellow inverted",
    menu_header("Cohort Explorer", class = "vertically fitted"),
    menu_item(uiOutput(ns("cohort_picked_ui")), class = "vertically fitted borderless"),
    menu(
      class = "yellow inverted right",
      menu_item(
        uiOutput(ns("cohort_picker_ui")) ,
        class = "vertically fitted"
      )
    )
  ), style = "margin-top:-20px;width:100%"),
  uiOutput(ns("cohort_summary"),class ="full-width"),
  tabset(tabs =
           list(
             list(menu = "Cohort Outcomes Explorer", content = cohortOutcomesExplorerUI(ns("cohort_outcomes_explorer"))),
             list(menu = "Cohort Parameter Explorer", content = tagList(
               form(
                 htmltools::tagAppendAttributes(fields(
                 htmltools::tagAppendAttributes(uiOutput(ns("parameter_picker_ui")),class="field"),
                 field(uiOutput(ns("field_picker"))),
                 field(uiOutput(ns("force_cast")))
               ),class="three my-10"),
               uiOutput(ns("fetch_action"))
               ),
               uiOutput(ns("plot_config_form")),
               uiOutput(ns("plot_strat_form")),
               uiOutput(ns("plot_clean_form")),
               uiOutput(ns("plot_action")),
               uiOutput(ns("graph_render")),
               uiOutput(ns("stratification_result"))
             ),style="min-height:50vh"),
             list(menu = "Cohort Patient Explorer", content = patientExplorerUI(ns("cohort_patient_explorer"))),
             list(menu = "Clinical Data Desc.", content = tagList(button(ns("reset_data_desc_button"), label = "Reset Data Desc.",class="green float-right mr-10"),clinicalDataDescUI(ns("clinical_data_desc"))))
           ),
         id = "tabset"
  )
  )
}
