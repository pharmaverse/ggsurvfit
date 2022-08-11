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

  # works with univariate model
  expect_error(
    ggsymbol_univariate <-
      survfit2(Surv(time, status) ~ 1, data = df_lung) |>
      ggsurvfit() +
      add_risktable(risktable_stats = "n.risk", risktable_group = "risktable_stats") +
      add_risktable_strata_symbol(vjust = 0.3),
    NA
  )
  vdiffr::expect_doppelganger(
    "add_risktable_strata_symbol-uni",
    ggsymbol_univariate
  )

  expect_message(
    print(survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
            ggsurvfit(size = 1) +
            add_confidence_interval() +
            add_risktable_strata_symbol(risktable_group = "risktable_stats")),
    "must be run before"
  )
  expect_message(
    print(survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
            ggsurvfit() +
            add_risktable() +
            add_risktable_strata_symbol()),
    "has been ignored"
  )
})
