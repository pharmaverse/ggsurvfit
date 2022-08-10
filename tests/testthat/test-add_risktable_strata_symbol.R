test_that("add_risktable_strata_symbol() works", {
  expect_error(
    p <-
      survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
      ggsurvfit(size = 1) +
      add_confidence_interval() +
      add_risktable(risktable_group = "risktable_stats"),
    NA
  )
  vdiffr::expect_doppelganger(
    "add_risktable_strata_symbol-default",
    p + add_risktable_strata_symbol()
  )
  vdiffr::expect_doppelganger(
    "add_risktable_strata_symbol-circle",
    p + add_risktable_strata_symbol(symbol = "\U25CF")
  )

  expect_message(
    print(survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
            ggsurvfit(size = 1) +
            add_confidence_interval() +
            add_risktable_strata_symbol(risktable_group = "risktable_stats")),
    "must be run before"
  )
  expect_message(
    print(survfit2(Surv(time, status) ~ 1, data = df_lung) |>
            ggsurvfit() +
            add_risktable() +
            add_risktable_strata_symbol()),
    "has been ignored"
  )
})
