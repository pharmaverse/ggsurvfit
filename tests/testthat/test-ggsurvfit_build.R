test_that("ggsurvfit_build() works", {
  p <-
    survfit2(Surv(time, status) ~ surg, df_colon) %>%
    ggsurvfit() +
    add_risktable()

  expect_error(
    built_p <- ggsurvfit_build(p, combine_plots = FALSE),
    NA
  )

  expect_true(inherits(built_p, "list"))
})
