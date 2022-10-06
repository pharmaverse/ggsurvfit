sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("ggsurvfit() works", {
  expect_error(
    lst_survfit2 <- list(sf1, sf2, sf3) %>% lapply(ggsurvfit),
    NA
  )
  vdiffr::expect_doppelganger("sf1-ggsurvfit", lst_survfit2[[1]])
  vdiffr::expect_doppelganger("sf2-ggsurvfit", lst_survfit2[[2]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit", lst_survfit2[[3]])


  expect_error(
    lst_survfit2_risk <-
      list(sf1, sf2, sf3) %>% lapply(function(x) ggsurvfit(x, type = "risk")),
    NA
  )
  vdiffr::expect_doppelganger("sf1-ggsurvfit_risk", lst_survfit2_risk[[1]])
  vdiffr::expect_doppelganger("sf2-ggsurvfit_risk", lst_survfit2_risk[[2]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit_risk", lst_survfit2_risk[[3]])

  expect_error(
    lst_survfit2_cumhaz <-
      list(sf1, sf2, sf3) %>% lapply(function(x) ggsurvfit(x, type = "cumhaz")),
    NA
  )
  vdiffr::expect_doppelganger("sf1-ggsurvfit_cumhaz", lst_survfit2_cumhaz[[1]])
  vdiffr::expect_doppelganger("sf2-ggsurvfit_cumhaz", lst_survfit2_cumhaz[[2]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit_cumhaz", lst_survfit2_cumhaz[[3]])

  expect_error(
    lst_survfit2_custom <-
      list(sf1, sf2, sf3) %>% lapply(function(x) ggsurvfit(x, type = function(x) 1 - x)),
    NA
  )
  vdiffr::expect_doppelganger("sf1-ggsurvfit_custom", lst_survfit2_custom[[1]])
  vdiffr::expect_doppelganger("sf2-ggsurvfit_custom", lst_survfit2_custom[[2]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit_custom", lst_survfit2_custom[[3]])

  expect_error(
    lst_survfit2_linetype <- list(sf2, sf3) %>% lapply(ggsurvfit, linetype_aes = TRUE),
    NA
  )
  vdiffr::expect_doppelganger("sf2-ggsurvfit_linetype", lst_survfit2_linetype[[1]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit_linetype", lst_survfit2_linetype[[2]])

  expect_error(
    lst_survfit_KMunicate <-
      list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x, theme = theme_ggsurvfit_KMunicate())),
    NA
  )
  vdiffr::expect_doppelganger("sf1-ggsurvfit-KMunicate", lst_survfit_KMunicate[[1]])
  vdiffr::expect_doppelganger("sf2-ggsurvfit-KMunicate", lst_survfit_KMunicate[[2]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit-KMunicate", lst_survfit_KMunicate[[3]])

  # test that the variable names are stripped when using transformations
  vdiffr::expect_doppelganger(
    "sf2-ggsurvfit-strata-transformation",
    survfit2(Surv(time, status) ~ as.numeric(sex), df_lung) %>%
      ggsurvfit()
  )

  # test the default ADTTE x-axis label comes from PARAM column
  expect_equal(
    ggsurvfit(survfit2(Surv_CNSR() ~ 1, data = adtte)) %>%
      ggplot2::ggplot_build() %>%
      `[[`("plot") %>%
      `[[`("labels") %>%
      `[[`("x"),
    adtte[["PARAM"]] %>% unique()
  )

  # testing the default label when using Surv_CNSR() without a PARAM COLUMN
  expect_equal(
    survfit2(
      Surv_CNSR() ~ STR01L,
      data = adtte %>% dplyr::select(-c(PARAM, PARAMCD))
    ) %>%
      .default_x_axis_label(),
    "Time"
  )

  # check ADTTE PARAM usage
  df_param2 <-
    adtte %>%
    dplyr::mutate(PARAM = rep_len(c("PFS", "OS"), length.out = dplyr::n()))

  expect_message(
    survfit2(Surv_CNSR() ~ 1, data = df_param2),
    "usage is likely incorrect"
  )

  # PARAM will not be used as label because of incorrect usage
  expect_equal(
    survfit2(Surv_CNSR() ~ 1, data = df_param2) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      `[[`("plot") %>%
      `[[`("labels") %>%
      `[[`("x"),
    "Time"
  )

  # x-axis label comes from PARAM
  expect_equal(
    survfit2(Surv_CNSR() ~ PARAM, data = df_param2) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      `[[`("plot") %>%
      `[[`("labels") %>%
      `[[`("x"),
    "Time"
  )

  expect_error(ggsurvfit(mtcars))
  expect_error(survfit2(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>% ggsurvfit())
})
