test_that("multiplication works", {
  withr::local_options(list("ggsurvfit.switch-color-linetype" = TRUE))
  expect_error(
    ggci_strata_outcomes <-
      tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc(outcome = c("death from cancer", "death other causes")) +
      add_censor_mark() +
      add_quantile(y = 0.1),
    NA
  )
  expect_error(print(ggci_strata_outcomes), NA)

  expect_error(
    ggci_outcomes <-
      tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, tidycmprsk::trial) %>%
      ggcuminc(outcome = c("death from cancer", "death other causes")) +
      add_censor_mark() +
      add_quantile(y = 0.1),
    NA
  )
  expect_error(print(ggci_outcomes), NA)

  expect_error(
    ggci_strata <-
      tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc() +
      add_censor_mark() +
      add_quantile(y = 0.1),
    NA
  )
  expect_error(print(ggci_strata), NA)

  skip_on_ci()
  vdiffr::expect_doppelganger("ggci_strata_outcomes", ggci_strata_outcomes)
  vdiffr::expect_doppelganger("ggci_outcomes", ggci_outcomes)
  vdiffr::expect_doppelganger("ggci_strata", ggci_strata)
})
