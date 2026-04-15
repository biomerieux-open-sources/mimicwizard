clinicalDataDescUI <- function(id) {
  ns <- NS(id)

  tagList(uiOutput(ns("clin_table")),
          div(message_box(
            "Statistical Testing Warning",
            HTML(
              "The statistical tests on this page are provided by the <strong>tableone</strong> package.
    Tests are selected automatically based on the characteristics of the sample.
    Please keep in mind that MIMIC‑IV contains a very large number of hospital stays,
    which results in high statistical power. This power can also amplify existing biases.
    More information about the statistical tests used in <strong>tableone</strong> is available here:
    <a href='https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html'>https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html</a>"
            ),
            class = "info my-10"
          )))
}
