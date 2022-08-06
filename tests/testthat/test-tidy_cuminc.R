cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)

test_that("tidy_cuminc() works", {
  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>% lapply(tidy_cuminc),
    NA
  )

  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>% lapply(tidy_cuminc, times = 12:14),
    NA
  )

  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>% lapply(tidy_cuminc, times = 1),
    NA
  )

  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>% lapply(tidy_cuminc, times = 0),
    NA
  )
})

test_that("tidy_cuminc() throws appropriate errors", {
  expect_error(tidy_cuminc(mtcars))
  expect_message(tidy_cuminc(cuminc1, times = -5:5))
})
