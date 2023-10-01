sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("add_confidence_interval() works with ggsurvfit()", {
  expect_error(
    lst_survfit2_confidence_interval <-
      list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_confidence_interval()),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_confidence_interval(type = "lines")),
    NA
  )

  # skip_on_ci()
  vdiffr::expect_doppelganger("sf1-confidence_interval", lst_survfit2_confidence_interval[[1]])
  vdiffr::expect_doppelganger("sf2-confidence_interval", lst_survfit2_confidence_interval[[2]])
  vdiffr::expect_doppelganger("sf3-confidence_interval", lst_survfit2_confidence_interval[[3]])

})

test_that("add_confidence_interval() errors with ggsurvfit()", {
  expect_error(
    (mtcars %>%
       ggplot2::ggplot(ggplot2::aes(y = mpg, x = hp)) +
       add_confidence_interval()) %>%
      print()
  )

  expect_error(
    (mtcars %>%
       dplyr::mutate(conf.low = mpg, conf.high = mpg) %>%
       ggplot2::ggplot(ggplot2::aes(y = mpg, x = hp)) +
       add_confidence_interval()) %>%
      print()
  )

  expect_error(
    ggsurvfit(sf1) + add_confidence_interval(type = "not_a_type"),
  )
})




cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)


test_that("add_confidence_interval() works with ggcuminc()", {
  expect_error(
    lst_cuminc_confidence_interval <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x) + add_confidence_interval()),
    NA
  )

  # skip_on_ci()
  vdiffr::expect_doppelganger("cuminc1-confidence_interval", lst_cuminc_confidence_interval[[1]])
  vdiffr::expect_doppelganger("cuminc2-confidence_interval", lst_cuminc_confidence_interval[[2]])
  vdiffr::expect_doppelganger("cuminc3-confidence_interval", lst_cuminc_confidence_interval[[3]])
})

test_that("add_confidence_interval() works with ggcuminc() and multiple outcomes", {
  expect_error(
    lst_cuminc_confidence_interval_outcomes <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x, outcome = c("death from cancer", "death other causes")) + add_confidence_interval()),
    NA
  )

  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x) + add_confidence_interval(type = "lines")),
    NA
  )

  # only check on mac
  # skip_on_ci()
  vdiffr::expect_doppelganger("cuminc1-confidence_interval-all-outcomes",
                              lst_cuminc_confidence_interval_outcomes[[1]])
  vdiffr::expect_doppelganger("cuminc2-confidence_interval-all-outcomes",
                              lst_cuminc_confidence_interval_outcomes[[2]])
  vdiffr::expect_doppelganger("cuminc3-confidence_interval-all-outcomes",
                              lst_cuminc_confidence_interval_outcomes[[3]])
})

test_that("add_confidence_interval() errors with ggcuminc()", {
  expect_error(
    ggcuminc(cuminc1) + add_confidence_interval(type = "not_a_type"),
  )
})
