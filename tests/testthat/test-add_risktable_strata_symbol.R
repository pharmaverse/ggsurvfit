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
  # survfit2 - Factor-----------------------------------------------------------
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
  # survfit - Factor -----------------------------------------------------------
  expect_equal(
    survfit(Surv(time, status) ~ sex, data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("sex=Male" = "#F8766D",   # red
      "sex=Female" = "#00BFC4") # blue
  )
  # survfit2 - Numeric ---------------------------------------------------------
  expect_equal(
    survfit2(Surv(time, status) ~ as.numeric(sex), data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("1" = "#F8766D",   # red
      "2" = "#00BFC4") # blue
  )
  # survfit - Numeric ----------------------------------------------------------
  expect_equal(
    survfit(Surv(time, status) ~ as.numeric(sex), data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("as.numeric(sex)=1" = "#F8766D",   # red
      "as.numeric(sex)=2" = "#00BFC4") # blue
  )
  # survfit2 - Character -------------------------------------------------------
  expect_equal(
    survfit2(Surv(time, status) ~ as.character(sex), data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("Female" = "#F8766D",   # red
      "Male" = "#00BFC4") # blue
  )
  # survfit - Character --------------------------------------------------------
  expect_equal(
    survfit(Surv(time, status) ~ as.character(sex), data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("as.character(sex)=Female" = "#F8766D",   # red
      "as.character(sex)=Male" = "#00BFC4") # blue
  )
  # survfit2 - Glue ------------------------------------------------------------
  expect_equal(
    survfit2(Surv(time, status) ~ sex,
             data = df_lung %>% dplyr::mutate(sex = glue::glue("{as.character(sex)}"))) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c(Female = "#F8766D",   # red
      Male = "#00BFC4") # blue
  )
  # survfit - Glue -------------------------------------------------------------
  expect_equal(
    survfit(Surv(time, status) ~ sex,
             data = df_lung %>% dplyr::mutate(sex = glue::glue("{as.character(sex)}"))) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("sex=Female" = "#F8766D",   # red
      "sex=Male" = "#00BFC4") # blue
  )
  # survfit2 - Ordered factor --------------------------------------------------
  expect_equal(
    survfit2(Surv(time, status) ~ factor(sex, ordered = TRUE), data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("Male" = "#F8766D",   # red
      "Female" = "#00BFC4") # blue
  )
  # survfit - Ordered factor ---------------------------------------------------
  expect_equal(
    survfit(Surv(time, status) ~ factor(sex, ordered = TRUE), data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("factor(sex, ordered = TRUE)=Male" = "#F8766D",   # red
      "factor(sex, ordered = TRUE)=Female" = "#00BFC4") # blue
  )
  # survfit2 - factors whose levels sort in alphabetical order -----------------
  expect_equal(
    survfit2(Surv(time, status) ~ sex,
            data = df_lung %>% dplyr::mutate(sex = factor(sex, levels = c("Female", "Male")))) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("Female" = "#F8766D",   # red
      "Male" = "#00BFC4") # blue
  )
  # survfit - factors whose levels sort in alphabetical order ------------------
  expect_equal(
    survfit(Surv(time, status) ~ sex,
             data = df_lung %>% dplyr::mutate(sex = factor(sex, levels = c("Female", "Male")))) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("sex=Female" = "#F8766D",   # red
      "sex=Male" = "#00BFC4") # blue
  )
  # survfit2 - factors whose levels do not sort alphabetically -----------------
  expect_equal(
    survfit2(Surv(time, status) ~ sex,
            data = df_lung %>% dplyr::mutate(sex = factor(sex, levels = c("Male", "Female")))) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("Male" = "#F8766D",   # red
      "Female" = "#00BFC4") # blue
  )
  # survfit - factors whose levels do not sort alphabetically ------------------
  expect_equal(
    survfit(Surv(time, status) ~ sex,
             data = df_lung %>% dplyr::mutate(sex = factor(sex, levels = c("Male", "Female")))) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("sex=Male" = "#F8766D",   # red
      "sex=Female" = "#00BFC4") # blue
  )
  # survfit2 - factors with unobserved levels ----------------------------------
  expect_equal(
    survfit2(Surv(time, status) ~ ph.ecog, data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("Asymptomatic" = "#F8766D", # red
      "Symptomatic and ambulatory" = "#7CAE00", # green
      "In bed <50% of the day" = "#00BFC4", # blue
      "In bed > 50% of the day" = "#C77CFF") # purple
  )
  # survfit - factors with unobserved levels -----------------------------------
  expect_equal(
    survfit(Surv(time, status) ~ ph.ecog, data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("ph.ecog=Asymptomatic" = "#F8766D", # red
      "ph.ecog=Symptomatic and ambulatory" = "#7CAE00", # green
      "ph.ecog=In bed <50% of the day" = "#00BFC4", # blue
      "ph.ecog=In bed > 50% of the day" = "#C77CFF") # purple
  )
  # survfit2 - No strata level -------------------------------------------------
  expect_equal(
    survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("Overall" = "black") # black
  )
  # survfit - No strata level --------------------------------------------------
  expect_equal(
    survfit(Surv(time, status) ~ 1, data = df_lung) %>%
      ggsurvfit() %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("Overall" = "black") # black
  )
  # survfit2 - color_scale_manual ----------------------------------------------
  expect_equal(
    survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
      ggsurvfit(color = "#00BFC4") %>%
    ggplot2::ggplot_build() %>%
    .match_strata_level_to_color(
      risktable_group = "risktable_stats",
      risktable_symbol_args = list(symbol = "\U25AC")
    ),
    c("Overall" = "#00BFC4") # blue
  )
  # survfit - color_scale_manual ----------------------------------------------
  expect_equal(
    survfit(Surv(time, status) ~ 1, data = df_lung) %>%
      ggsurvfit(color = "#00BFC4") %>%
      ggplot2::ggplot_build() %>%
      .match_strata_level_to_color(
        risktable_group = "risktable_stats",
        risktable_symbol_args = list(symbol = "\U25AC")
      ),
    c("Overall" = "#00BFC4") # blue
  )

  # Add more tests using the following:
  # - survfit() and survfit2()
  # - strata variables that are type numeric, character, glue,
  #      factor, ordered factor, factors whose levels sort in alphabetical order,
  #      factors whose levels do not sort alphabetically,
  #      factors with unobserved levels
  # - no strata level, e.g. Surv(time, status) ~ 1
  # - when the colors have been changed by the user with `scale_color_manual()`
  # - when two or more levels have been assigned the same color

})
