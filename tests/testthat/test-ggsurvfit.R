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

  expect_error(ggsurvfit(mtcars))
})
