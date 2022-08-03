# DATA -------------------------------------------------------------------------

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


usethis::use_data(df_lung, overwrite = TRUE, internal = FALSE)
