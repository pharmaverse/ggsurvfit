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
      survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
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


test_that(".match_strata_level_to_color() works", {
  expect_equal(
    survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c(Male = "#F8766D",   # red
      Female = "#00BFC4") # blue
  )

  # Add more tests using the following:
  # - survfit() and survfit2()
  # - strata variables that are type numeric, character, glue,
  #      factor, ordered factor, factors whose levels sort in alphabetical order,
  #      factors whose levels do not sort alphabetically,
  #      factors with unobserved levels
  # - no strata level, e.g. sSurv(time, status) ~ 1
  # - when the colors have been changed by the user with `scale_color_manual()`
  # - when two or more levels have been assigned the same color

})
