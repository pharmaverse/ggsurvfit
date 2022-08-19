# COLON DATA -------------------------------------------------------------------
df_colon <-
  survival::colon |>
  dplyr::filter(etype == 1) |>
  dplyr::select(-study, -etype) |>
  dplyr::mutate(
    rx = factor(rx,
                levels = c("Obs", "Lev", "Lev+5FU"),
                labels = c("Observation", " Levamisole", "Levamisole+5-FU")),
    sex = factor(sex, levels = 0:1, labels = c("Female", "Male")),
    obstruct =
      factor(
        obstruct,
        levels = 0:1,
        labels = c("Not Obstructed", "Obstructed")
      ),
    adhere =
      factor(
        adhere,
        levels = 0:1,
        labels = c("Not Adhered", "Adhered")
      ),
    time = time / 365.25,
    differ =
      factor(
        differ,
        levels = 1:3,
        labels = c("Well", "Moderate", "Poor")
      ),
    extent =
      factor(
        extent,
        levels = 1:4,
        labels = c("Submucosa", "Muscle", "Serosa", "Contiguous Structures")
      ),
    surg =
      factor(
        surg,
        levels = 0:1,
        labels = c("Limited Time Since Surgery", "Extended Time Since Surgery")
      )
  ) %>%
  dplyr::as_tibble()

attr(df_colon$id, "label") <- "ID"
attr(df_colon$rx, "label") <- "Treatment"
attr(df_colon$sex, "label") <- "Sex"
attr(df_colon$age, "label") <- "Age, years"
attr(df_colon$obstruct, "label") <- "Tumor-related Obstruction"
attr(df_colon$perfor, "label") <- "Colon Perforated"
attr(df_colon$adhere, "label") <- "Adherence to Nearby Organs"
attr(df_colon$nodes, "label") <- "No. Positive Lymph Nodes"
attr(df_colon$time, "label") <- "Follow-up time, years"
attr(df_colon$status, "label") <- "Recurrence Status"
attr(df_colon$differ, "label") <- "Tumor Differentiation"
attr(df_colon$extent, "label") <- "Extent of Spread"
attr(df_colon$surg, "label") <- "Time from Surgery to Treatment"
attr(df_colon$node4, "label") <- "More than 4 Positive Nodes"


# LUNG DATA --------------------------------------------------------------------

# recode values and add labels to survival::lung
df_lung <-
  survival::lung |>
  dplyr::mutate(
    sex =
      factor(sex, levels = 1:2, labels = c("Male", "Female")),
    time = time / 365.25 * 12,
    ph.ecog =
      factor(ph.ecog,
        levels = 0:4,
        labels = c(
          "Asymptomatic",
          "Symptomatic and ambulatory",
          "In bed <50% of the day",
          "In bed > 50% of the day",
          "Bedbound"
        )
      )
  ) %>%
  dplyr::as_tibble()

attr(df_lung$inst, "label") <- "Institution code"
attr(df_lung$time, "label") <- "Survival time, months"
attr(df_lung$status, "label") <- "Censoring status, 1=censored, 2=dead"
attr(df_lung$age, "label") <- "Age"
attr(df_lung$sex, "label") <- "Sex"
attr(df_lung$ph.ecog, "label") <- "ECOG Performance Status (Physician)"
attr(df_lung$ph.karno, "label") <- "Karnofsky performance score (Physician)"
attr(df_lung$pat.karno, "label") <- "Karnofsky performance score (Patient)"
attr(df_lung$meal.cal, "label") <- "Calories consumed"
attr(df_lung$wt.loss, "label") <- "Weight loss, lbs"

# ADTTE DATA -------------------------------------------------------------------
#source data https://github.com/VIS-SIG/Wonderful-Wednesdays/tree/master/data/2020/2020-04-08

# load data - local version
# variable names toLowerCase
adtte <-
  readr::read_csv('data-raw/2020-04-08-psi-vissig-adtte.csv',
                  show_col_types = FALSE) |>
  dplyr::as_tibble() |>
  dplyr::mutate(AVAL = AVAL / 365.25)

# attach var labels
attr(adtte$STUDYID, "label") <- "Study identifier"
attr(adtte$USUBJID, "label") <- "Unique patient identifier"
attr(adtte$AGE, "label") <- "Age at randomization [years]"
attr(adtte$STR01, "label") <- "Hormone receptor"
attr(adtte$STR01N, "label") <- "Hormone receptor positive (Numeric)"
attr(adtte$STR01L, "label") <- "Hormone receptor positive at randomization"
attr(adtte$STR02, "label") <- "Prior Radiotherapy at randomization"
attr(adtte$STR02N, "label") <- "Prior Radiotherapy at randomization (Numeric)"
attr(adtte$STR02L, "label") <- "Prior Radiotherapy at randomization"
attr(adtte$TRT01P, "label") <- "Planned treatment"
attr(adtte$TRT01PN, "label") <- "Planned treatment group assigned at randomization (Numeric)"
attr(adtte$PARAM, "label") <- "Progression free survival"
attr(adtte$PARAMCD, "label") <- "PFS"
attr(adtte$AVAL, "label") <- "Folow-up time, years"
attr(adtte$CNSR, "label") <- "Censoring flag (0 = Event, 1 = censored)"
attr(adtte$EVNTDESC, "label") <- "Event description"
attr(adtte$CNSDTDSC, "label") <- " Censoring description"
attr(adtte$DCTREAS, "label") <- "Discontinuation from study reason"



usethis::use_data(adtte, df_lung, df_colon, overwrite = TRUE, internal = FALSE)
