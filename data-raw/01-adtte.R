#source data https://github.com/VIS-SIG/Wonderful-Wednesdays/tree/master/data/2020/2020-04-08

# load data - local version
# variable names toLowerCase
adtte <-
  readr::read_csv('data-raw/2020-04-08-psi-vissig-adtte.csv',
                  show_col_types = FALSE) |>
  #janitor::clean_names() |>
  dplyr::as_tibble()


# attach var labels
attr(adtte$STUDYID, "label") <- "Study identifier"
attr(adtte$USUBJID, "label") <- "Unique patient identifier"
attr(adtte$AGE, "label") <- "Age at randomisation [years]"
attr(adtte$STR01, "label") <- "Hormone receptor status at randomisation"
attr(adtte$STR01N, "label") <- "Hormone receptor positive (Numeric)"
attr(adtte$STR01L, "label") <- "Hormone receptor positive at randomisation"
attr(adtte$STR02, "label") <- "Prior Radiotherapy at randomisationr"
attr(adtte$STR02N, "label") <- "Prior Radiotherapy at randomisation (Numeric)"
attr(adtte$STR02L, "label") <- "Prior Radiotherapy at randomisation"
attr(adtte$TRT01P, "label") <- "Planned treatment assigned at randomisation"
attr(adtte$TRT01PN, "label") <- "Planned treatment assigned at randomisation (Numeric)"
attr(adtte$PARAM, "label") <- "Progression free survival"
attr(adtte$PARAMCD, "label") <- "PFS"
attr(adtte$AVAL, "label") <- "Follow up time [days]"
attr(adtte$CNSR, "label") <- "Censoring flag (0 = Event, 1 = censored)"
attr(adtte$EVNTDESC, "label") <- "Event description"
attr(adtte$CNSDTDSC, "label") <- " Censoring description"
attr(adtte$DCTREAS, "label") <- "Discontinuation from study reason"


# save to data
usethis::use_data(adtte, overwrite = TRUE, internal = FALSE)
