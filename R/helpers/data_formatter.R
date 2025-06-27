generic_data_formatter <- function(all_events,patient_general_information,icd_data){
  patient_string <- ""
  patient_string <- paste(patient_string,
                          "Gender : ",
                          patient_general_information[['gender']][1],
                          "\n")
  patient_string <- paste(patient_string,
                          "Age : ",
                          patient_general_information[['age']][1],
                          "\n")
  patient_string <- paste(
    patient_string,
    "Charlson Comorbidity Index : ",
    patient_general_information[['charlson_comorbidity_index']][1],
    "\n"
  )
  patient_string <- paste(patient_string,
                          "SOFA : ",
                          patient_general_information[['sofa']][1],
                          "\n")
  patient_string <- paste(
    patient_string,
    "Glasgow Score Minimum : ",
    patient_general_information[['gcs_min']][1],
    "\n"
  )

  patient_string <- paste(
    patient_string,
    icd_data %>% select("icd_code", "icd_version", "long_title") %>% {
      paste(.$icd_code,
            paste0("v", .$icd_version),
            .$long_title)
    } %>% paste(collapse = "\n")
  )

  # Dirty code :'(
  target =  all_events %>%
    filter(category != "User imported") %>%
    select(itemid, starttime, category, param_type, label, value) %>%
    arrange(itemid, starttime) %>%
    group_by(itemid, starttime, category, param_type, label) %>%
    slice_head(n = 1)

  pivoted = pivot_wider(
    target,
    names_from = c(category, param_type, label, itemid),
    names_glue = "{category} {param_type} {label} {itemid}",
    values_from = value,
    values_fill = NA
  ) %>%
    ungroup() %>%
    select(-starttime)

  for (n in names(pivoted)) {
    data <- pivoted[[n]][!is.na(pivoted[[n]])]
    if (length(data) > 0) {
      patient_string <- paste(patient_string,
                              n,
                              " : ",
                              paste(data, collapse =
                                      " "),
                              "\n")
    }

  }
  patient_string
}
