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

  # checking that the magrittr pipe env is handled correctly, and the data can be accessed
  expect_error(
    sf <- adtte %>% survfit2(Surv_CNSR() ~ STR01, data = .),
    NA
  )
  expect_equal(
    ggsurvfit(sf) %>%
      {suppressWarnings(ggplot2::ggplot_build(.))} %>%
      `[[`("plot") %>%
      `[[`("labels") %>%
      `[[`("x"),
    adtte[["PARAM"]] %>% unique()
  )
  expect_equal(
    survfit2_p(sf),
    survival::survdiff(Surv_CNSR() ~ STR01, data = adtte) %>%
      broom::glance() %>%
      dplyr::pull(p.value) %>%
      format_p() %>%
      {paste0("p=", .)}
  )
  expect_equal(
    sf %>%
      tidy_survfit() %>%
      dplyr::pull(strata_label) %>%
      unique(),
    attr(adtte[["STR01"]], "label")
  )

  expect_error(survfit2(formula = mtcars))
  expect_error(survfit2())
})
