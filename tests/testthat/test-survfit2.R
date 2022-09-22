test_that("survfit2() works", {
  expect_identical(
    survfit2(Surv(time, status) ~ 1, data = survival::lung) %>%
      modifyList(val = list(call = NULL, .Environment = NULL)) %>%
      unclass(),
    survival::survfit(Surv(time, status) ~ 1, data = survival::lung) %>%
      modifyList(val = list(call = NULL)) %>%
      unclass()
  )

  expect_identical(
    survfit2(Surv(time, status) ~ 1, data = survival::lung, cluster = ph.karno) %>%
      modifyList(val = list(call = NULL, .Environment = NULL)) %>%
      unclass(),
    survival::survfit(Surv(time, status) ~ 1, data = survival::lung, cluster = ph.karno) %>%
      modifyList(val = list(call = NULL)) %>%
      unclass()
  )

  expect_identical(
    survfit2(Surv(survival::lung$time, survival::lung$status) ~ 1) %>%
      modifyList(val = list(call = NULL, .Environment = NULL)) %>%
      unclass(),
    survival::survfit(Surv(survival::lung$time, survival::lung$status) ~ 1) %>%
      modifyList(val = list(call = NULL)) %>%
      unclass()
  )

  expect_message(
    df_lung %>% survfit2(Surv(time, status) ~ sex, data = .)
  )

  expect_error(survfit2(formula = mtcars))
  expect_error(survfit2())
})
