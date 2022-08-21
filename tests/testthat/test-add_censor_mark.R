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
  vdiffr::expect_doppelganger("sf1-censor_mark", lst_survfit2_censor_mark[[1]])
  vdiffr::expect_doppelganger("sf2-censor_mark", lst_survfit2_censor_mark[[2]])
  vdiffr::expect_doppelganger("sf3-censor_mark", lst_survfit2_censor_mark[[3]])

})

test_that("add_censor_mark() errors with ggsurvfit()", {
  expect_warning(
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
  vdiffr::expect_doppelganger("cuminc1-censor_mark", lst_cuminc_censor_mark[[1]])
  vdiffr::expect_doppelganger("cuminc2-censor_mark", lst_cuminc_censor_mark[[2]])
  vdiffr::expect_doppelganger("cuminc3-censor_mark", lst_cuminc_censor_mark[[3]])

  expect_error(
    lst_cuminc_censor_mark_outcome <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x, outcome = c("death from cancer", "death other causes")) + add_censor_mark()),
    NA
  )
  vdiffr::expect_doppelganger("cuminc1-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[1]])
  vdiffr::expect_doppelganger("cuminc2-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[2]])
  vdiffr::expect_doppelganger("cuminc3-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[3]])
})

test_that("add_censor_mark() works with ggcuminc()", {
  skip_on_os("linux")

  expect_error(
    lst_cuminc_censor_mark_outcome <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x, outcome = c("death from cancer", "death other causes")) + add_censor_mark()),
    NA
  )
  vdiffr::expect_doppelganger("cuminc1-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[1]])
  vdiffr::expect_doppelganger("cuminc2-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[2]])
  vdiffr::expect_doppelganger("cuminc3-censor_mark-all-outcomes", lst_cuminc_censor_mark_outcome[[3]])
})

