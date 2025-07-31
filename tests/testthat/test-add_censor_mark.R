sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("add_censor_mark() works with ggsurvfit()", {
  expect_error(
    lst_survfit2_censor_mark <-
      list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_censor_mark()),
    NA
  )

  # only check on mac
  skip_on_ci()
  vdiffr::expect_doppelganger("sf1-censor_mark", lst_survfit2_censor_mark[[1]])
  vdiffr::expect_doppelganger("sf2-censor_mark", lst_survfit2_censor_mark[[2]])
  vdiffr::expect_doppelganger("sf3-censor_mark", lst_survfit2_censor_mark[[3]])

})

test_that("add_censor_mark() errors with ggsurvfit()", {
  expect_error(
    (mtcars %>%
       ggplot2::ggplot(ggplot2::aes(y = mpg, x = hp)) +
       add_censor_mark()) %>%
      print()
  )
})




cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)


test_that("add_censor_mark() works with ggcuminc()", {
  expect_error(
    lst_cuminc_censor_mark <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x) + add_censor_mark()),
    NA
  )

  # only check on mac
  skip_on_ci()
  vdiffr::expect_doppelganger("cuminc1-censor_mark", lst_cuminc_censor_mark[[1]])
  vdiffr::expect_doppelganger("cuminc2-censor_mark", lst_cuminc_censor_mark[[2]])
  vdiffr::expect_doppelganger("cuminc3-censor_mark", lst_cuminc_censor_mark[[3]])
})

test_that("add_censor_mark() works with ggcuminc()", {
  expect_error(
    lst_cuminc_censor_mark_outcome <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x, outcome = c("death from cancer", "death other causes")) + add_censor_mark()),
    NA
  )

  # only check on mac
  skip_on_ci()
  vdiffr::expect_doppelganger("cuminc1-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[1]])
  vdiffr::expect_doppelganger("cuminc2-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[2]])
  vdiffr::expect_doppelganger("cuminc3-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[3]])
})

test_that("add_censor_mark() gives informative error with weighted survival data", {
  # Create weighted survival data that will produce non-integer n.censor
  df_weighted <- df_lung
  df_weighted$weights <- runif(nrow(df_lung), 0.1, 2.0)  # Random weights

  # Create a weighted survfit object
  survfit_weighted <- survfit2(Surv(time, status) ~ sex,
                              data = df_weighted,
                              weights = df_weighted$weights)

  # Create the base ggsurvfit plot
  p <- ggsurvfit(survfit_weighted)

  # Test that add_censor_mark() throws the expected error with key message components
  expect_error(
    p + add_censor_mark(),
    "cannot be used with weighted survival data"
  )

  expect_error(
    p + add_censor_mark(),
    "Round the censoring counts"
  )

  expect_error(
    p + add_censor_mark(),
    "tidyr::uncount"
  )
})

test_that("add_censor_mark() works with integer-rounded weighted data", {
  # Create weighted survival data
  df_weighted <- df_lung
  df_weighted$weights <- runif(nrow(df_lung), 0.1, 2.0)

  # Create a weighted survfit object and round n.censor (the workaround solution)
  survfit_weighted <- survfit2(Surv(time, status) ~ sex,
                              data = df_weighted,
                              weights = df_weighted$weights)
  survfit_weighted$n.censor <- round(survfit_weighted$n.censor)

  # This should work without error after rounding
  expect_error(
    ggsurvfit(survfit_weighted) + add_censor_mark(),
    NA
  )
})

