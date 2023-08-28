sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("theme_risktable works", {
  expect_error(
    lst_survfit2_default <-
      list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_risktable(theme = theme_risktable_default())),
    NA
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("sf1-ggsurvfit_theme_risktable_default", lst_survfit2_default[[1]])
  vdiffr::expect_doppelganger("sf2-ggsurvfit_theme_risktable_default", lst_survfit2_default[[2]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit_theme_risktable_default", lst_survfit2_default[[3]])

  expect_error(
    lst_survfit2_boxed <-
      list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_risktable(theme = theme_risktable_boxed())),
    NA
  )
  vdiffr::expect_doppelganger("sf1-ggsurvfit_theme_risktable_boxed", lst_survfit2_boxed[[1]])
  vdiffr::expect_doppelganger("sf2-ggsurvfit_theme_risktable_boxed", lst_survfit2_boxed[[2]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit_theme_risktable_boxed", lst_survfit2_boxed[[3]])

  expect_error(
    lst_survfit2_boxed_symbol <-
      list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_risktable(theme = theme_risktable_boxed(), risktable_group = "risktable_stats") + add_risktable_strata_symbol()),
    NA
  )
  vdiffr::expect_doppelganger("sf1-ggsurvfit_theme_risktable_boxed_symbol", lst_survfit2_boxed_symbol[[1]])
  vdiffr::expect_doppelganger("sf2-ggsurvfit_theme_risktable_boxed_symbol", lst_survfit2_boxed_symbol[[2]])
  vdiffr::expect_doppelganger("sf3-ggsurvfit_theme_risktable_boxed_symbol", lst_survfit2_boxed_symbol[[3]])
})
