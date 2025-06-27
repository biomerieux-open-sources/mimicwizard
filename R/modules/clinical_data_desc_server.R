clinicalDataDescServer <-
  function(id,
           database = NULL,
           reactive_desc_data = NULL) {
    moduleServer(id,
                 function(input, output, session) {
                   ns <- session$ns

                   if (!is.null(database) &
                       !is.null(reactive_desc_data)) {
                     output$clin_table <- renderUI({
                       datatable_list <- list()
                        if(length(reactive_desc_data()) > 0){
                          for (i in 1:length(reactive_desc_data())) {
                            clin_data <-
                              reactive_desc_data()[[i]] %>% select(-c(stay_id, interval))
                            datatable_list <- append(datatable_list,
                                                     list(renderDT({
                                                       suppressWarnings(datatable(
                                                         as.data.frame(
                                                           print(
                                                             CreateTableOne(data = clin_data, strata = c("strat")),
                                                             showAllLevels = TRUE,
                                                             varLabels = TRUE,
                                                             printToggle = FALSE,
                                                             nonnormal = TRUE
                                                           )
                                                         ),
                                                         options = list(
                                                           dom = 't',
                                                           paging = FALSE,
                                                           searching = FALSE
                                                         )
                                                       ))
                                                     })))
                          }
                          tagList(datatable_list)
                        }

                     })

                   }
                 })
  }
