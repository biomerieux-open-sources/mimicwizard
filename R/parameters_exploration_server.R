parametersExplorationServer <- function(id, database=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      if (!is.null(database)){


        withProgress(message = "Acquiring events list from MIMIC", {
          events_summary <- dplyr::tbl(database(), in_schema("public", "events_summary")) %>% collect()
          setProgress(value = 5/5, message = "Rendering stats in datatable")



        output$params_list <- DT::renderDataTable(events_summary,
                                                  extensions = c(
                                                    "Buttons",
                                                    "RowGroup",
                                                    "Scroller"
                                                    # "FixedColumns", ## is on disable filter options
                                                    # "FixedHeader",
                                                    # "KeyTable"
                                                  ),
                                                  selection = "none",
                                                  rownames = FALSE,
                                                  filter = list(position = 'top'),
                                                  options = list(
                                                    # scrollY = 200,
                                                    # scroller = TRUE,
                                                    # keys = TRUE,
                                                    dom = 'ftrip',
                                                    pageLength = 10,
                                                    autoWidth = F,
                                                    # buttons = list('colvis'),
                                                    # scrollCollapse = T,
                                                    scrollX = T
                                                    #rowGroup = list(dataSrc = 0)
                                                    # fixedColumns = list(leftColumns = 3),
                                                    # fixedHeader = TRUE
                                                  ),
                                                  escape = FALSE)
        })
        # events_summary_view_sql_generator(database)
      }
      }
  )
}


#########
###      This function contains code used for events_summary MATERIALIZED VIEW generation
#########
events_summary_view_sql_generator <- function(database){
  patients <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "patients"))
  admissions <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "admissions"))
  d_items <- dplyr::tbl(database(), in_schema("mimiciv_icu", "d_items") )
  d_labitems <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "d_labitems") )

  procedureevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "procedureevents") ) %>%
    inner_join(d_items, by="itemid")%>%
    select(c("subject_id",
             "hadm_id",
             "stay_id",
             "itemid",
             "label",
             "abbreviation",
             "linksto",
             "category",
             "unitname",
             "param_type",
             "lownormalvalue",
             "highnormalvalue",
             "value",
             "valueuom",
             "starttime",
             "endtime")) %>%
    mutate(starttime = as.POSIXct(starttime),endtime=as.POSIXct(endtime))

  ingredientevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "ingredientevents") ) %>%
    inner_join(d_items, by="itemid")%>%
    select(c("subject_id",
             "hadm_id",
             "stay_id",
             "itemid",
             "label",
             "abbreviation",
             "linksto",
             "category",
             "unitname",
             "param_type",
             "lownormalvalue",
             "highnormalvalue",
             "amount",
             "amountuom",
             "starttime",
             "endtime")) %>%
    rename(value = amount, valueuom = amountuom) %>%
    mutate(starttime = as.POSIXct(starttime), endtime=as.POSIXct(endtime))
  inputevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "inputevents") ) %>%
    inner_join(d_items, by="itemid")%>%
    select(c("subject_id",
             "hadm_id",
             "stay_id",
             "itemid",
             "label",
             "abbreviation",
             "linksto",
             "category",
             "unitname",
             "param_type",
             "lownormalvalue",
             "highnormalvalue",
             "amount",
             "amountuom",
             "starttime",
             "endtime")) %>%
    rename(value = amount, valueuom = amountuom)%>%
    mutate(starttime = as.POSIXct(starttime),endtime=as.POSIXct(endtime))

  outputevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "outputevents") ) %>%
    inner_join(d_items, by="itemid")%>%
    select(c("subject_id",
             "hadm_id",
             "stay_id",
             "itemid",
             "label",
             "abbreviation",
             "linksto",
             "category",
             "unitname",
             "param_type",
             "lownormalvalue",
             "highnormalvalue",
             "value",
             "valueuom",
             "charttime")) %>%
    rename(starttime = charttime)%>%
    mutate(starttime = as.POSIXct(starttime),endtime=as.POSIXct(NULL))

  datetimeevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "datetimeevents") )  %>%
    inner_join(d_items, by="itemid")%>%
    select(c("subject_id",
             "hadm_id",
             "stay_id",
             "itemid",
             "label",
             "abbreviation",
             "linksto",
             "category",
             "unitname",
             "param_type",
             "lownormalvalue",
             "highnormalvalue",
             "value",
             "valueuom",
             "charttime")) %>%
    rename(starttime = charttime) %>%
    mutate(value = NULL, starttime = as.POSIXct(starttime),endtime=as.POSIXct(NULL))

  chartevents <- dplyr::tbl(database(), in_schema("mimiciv_icu", "chartevents")) %>%
    inner_join(d_items, by="itemid")%>%
    select(c("subject_id",
             "hadm_id",
             "stay_id",
             "itemid",
             "label",
             "abbreviation",
             "linksto",
             "category",
             "unitname",
             "param_type",
             "lownormalvalue",
             "highnormalvalue",
             "valuenum",
             "valueuom",
             "charttime")) %>%
    rename(value = valuenum,starttime = charttime) %>%
    mutate(starttime = as.POSIXct(starttime),endtime=as.POSIXct(NULL))

  labevents <- dplyr::tbl(database(), in_schema("mimiciv_hosp", "labevents") ) %>%
    inner_join(d_labitems, by="itemid" )%>%
    transmute(subject_id,
             hadm_id,
             stay_id=NULL,
             itemid,
             label,
             abbreviation=NULL,
             linksto="labevents",
             category,
             unitname=NULL,
             param_type=fluid,
             lownormalvalue=NULL,
             highnormalvalue=NULL,
             value=valuenum,
             valueuom,
             starttime=as.POSIXct(charttime),
             endtime=as.POSIXct(NULL)
             )

  microbiologyresultsevents <- dplyr::tbl(database(), in_schema("public", "microbiologyresultsevents") ) %>%
    transmute(subject_id,
              hadm_id,
              stay_id,
              itemid = test_itemid,
              label=label,
              abbreviation = NULL,
              linksto = "microbiologyresultsevents",
              category = "Microbiology",
              unitname = NULL,
              param_type = NULL,
              lownormalvalue = NULL,
              highnormalvalue = NULL,
              value=valuenum,
              valueuom=as.character(microevent_id),
              starttime=charttime,
              endtime=NULL) %>%
    mutate(starttime = as.POSIXct(starttime),endtime=as.POSIXct(NULL))

  patient_events_list <- Reduce(union_all, list(procedureevents, ingredientevents, inputevents, outputevents, datetimeevents, chartevents, labevents,microbiologyresultsevents))


  events_stats <- list()
  events_stats$evt_cnt <- patient_events_list %>%
    count(itemid,label, name = 'evt_cnt')

  events_stats$stay_cnt <- patient_events_list %>%
    group_by(itemid,label) %>%
    summarise(stay_cnt = n_distinct(stay_id))

  events_stats$hadm_cnt <- patient_events_list %>%
    group_by(itemid,label) %>%
    summarise(hadm_cnt = n_distinct(hadm_id))

  events_stats$patient_cnt <- patient_events_list %>%
    group_by(itemid,label) %>%
    summarise(patient_cnt = n_distinct(subject_id))

  events_summary <- Reduce(inner_join,events_stats)

  cat(capture.output(events_summary %>% show_query(), type="output"), file="sql_query.txt")
}


