sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("add_risktable() works with ggsurvfit()", {
  expect_error(
    lst_survfit2_risktable <-
      list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_risktable()),
    NA,
  )
  expect_error(
    lst_survfit2_risktable %>%
      lapply(function(x) print(x)),
    NA,
  )


  expect_error(
    list(sf1, sf2, sf3) %>%
      lapply(
        function(x) {
          (ggsurvfit(x) +
             add_risktable(risktable_stats = c("n.risk", "cum.event"), stats_label = list(cum.event = "CUM EVENTS"))) %>%
            print()
        }
      ),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>%
      lapply(
        function(x) {
          (ggsurvfit(x) +
             add_risktable(risktable_stats = c("n.risk", "cum.event"), stats_label = c("N RISK", "CUM EVENTS"))) %>%
            print()
        }
      ),
    NA,
  )

  expect_error(
    list(sf2, sf3) %>%
      lapply(function(x) (ggsurvfit(x) + add_risktable(risktable_group = "strata")) %>% print()),
    NA,
  )

  expect_error(
    list(sf2, sf3) %>%
      lapply(function(x) (ggsurvfit(x) + add_risktable(combine_groups = TRUE)) %>% print()),
    NA,
  )

  expect_error(
    risktable_overall1 <-
      sf1 %>%
      ggsurvfit() +
      add_risktable(risktable_stats = "n.risk",
                    risktable_group = "risktable_stats"),
    NA
  )
  expect_error(print(risktable_overall1), NA)

  expect_error(
    risktable_overall2 <-
      sf1 %>%
      ggsurvfit() +
      add_risktable(risktable_stats = c("n.risk", "cum.event"),
                    risktable_group = "risktable_stats"),
    NA
  )
  expect_error(print(risktable_overall2), NA)

  # when weights are present, the risktable Ns should be rounded to nearest integer
  expect_error(
    risktable_with_weights <-
      survfit2(
        formula = Surv(time, status) ~ 1,
        data = df_lung,
        weights = abs(scale(age))
      ) %>%
      ggsurvfit() +
      add_risktable(
        risktable_stats = c("{round(n.risk)}", "{round(cum.event)}"),
        stats_label = c("At Risk", "Events")
      ),
    NA
  )
  expect_error(print(risktable_with_weights), NA)

  expect_error(
    `sf1-risktable-height` <-
      sf1 %>% ggsurvfit() +
      add_risktable(risktable_height = 0.40),
    NA
  )
  expect_error(print(`sf1-risktable-height`), NA)

  expect_error(
    p_with_dup_axis <-
      sf2 %>%
      ggsurvfit() +
      add_risktable() +
      ggplot2::scale_x_continuous(
        name = "Time (Months)",
        breaks = seq(0, 30, 6),
        sec.axis = ggplot2::sec_axis(~ . / 12, name = "Time (Years)")
      ),
    NA
  )
  expect_error(print(p_with_dup_axis), NA)

  # only check on mac
  skip_on_ci()
  vdiffr::expect_doppelganger("sf1-risktable", lst_survfit2_risktable[[1]])
  vdiffr::expect_doppelganger("sf2-risktable", lst_survfit2_risktable[[2]])
  vdiffr::expect_doppelganger("sf3-risktable", lst_survfit2_risktable[[3]])

  vdiffr::expect_doppelganger("sf1-risktable-height", `sf1-risktable-height`)
  vdiffr::expect_doppelganger("add_risktable-overall1", risktable_overall1)
  vdiffr::expect_doppelganger("add_risktable-overall2", risktable_overall2)
  vdiffr::expect_doppelganger("add_risktable-weights", risktable_with_weights)
  vdiffr::expect_doppelganger("sf2-risktable-duplicated-axis", p_with_dup_axis)
})

test_that("add_risktable() throws error messages", {
  expect_error(
    (ggsurvfit(sf1) +
       add_risktable(
         risktable_stats = c("n.risk", "cum.event"),
         stats_label = "CUM EVENTS"
       )) %>%
      print()
  )

  expect_error(
    ggsurvfit_build(
      ggsurvfit(sf1) +
        add_risktable(risktable_height = letters)
    )
  )
})


cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)

test_that("add_risktable() works with ggcuminc()", {
  expect_error(
    lst_cuminc_risktable <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x) + add_risktable()),
    NA,
  )
  expect_error(
    lst_cuminc_risktable %>%
      lapply(function(x) print(x)),
    NA,
  )

  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>%
      lapply(
        function(x) {
          (ggcuminc(x) +
             add_risktable(risktable_stats = c("n.risk", "cum.event"), stats_label = list(cum.event = "CUM EVENTS"))) %>%
            print()
        }
      ),
    NA,
  )

  expect_error(
    list(cuminc2, cuminc3) %>%
      lapply(function(x) (ggcuminc(x) + add_risktable(risktable_group = "strata")) %>% print()),
    NA,
  )

  expect_error(
    list(cuminc2, cuminc3) %>%
      lapply(function(x) (ggcuminc(x) + add_risktable(combine_groups = TRUE)) %>% print()),
    NA,
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("cuminc1-risktable", lst_cuminc_risktable[[1]])
  vdiffr::expect_doppelganger("cuminc2-risktable", lst_cuminc_risktable[[2]])
  vdiffr::expect_doppelganger("cuminc3-risktable", lst_cuminc_risktable[[3]])
})

test_that("add_risktable() works with ggcuminc() and multiple outcomes", {
  expect_error(
    lst_cuminc_risktable_outcomes <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x, outcome = c("death from cancer", "death other causes")) + add_risktable()),
    NA,
  )
  expect_error(
    lst_cuminc_risktable_outcomes %>% lapply(function(x) print(x)),
    NA
  )

  # only check on mac
  skip_on_ci()
  vdiffr::expect_doppelganger("cuminc1-risktable-all-outcomes", lst_cuminc_risktable_outcomes[[1]])
  vdiffr::expect_doppelganger("cuminc2-risktable-all-outcomes", lst_cuminc_risktable_outcomes[[2]])
  vdiffr::expect_doppelganger("cuminc3-risktable-all-outcomes", lst_cuminc_risktable_outcomes[[3]])
})

test_that("add_risktable() throws messages", {
  expect_error(
    (ggcuminc(cuminc1) +
       add_risktable(
         risktable_stats = c("n.risk", "cum.event"),
         stats_label = "CUM EVENTS"
       )) %>%
      print()
  )

  expect_message(
    print(
      survfit2(Surv(AVAL, 1 - CNSR) ~ STR01, data = adtte) %>%
        ggsurvfit() +
        add_confidence_interval() +
        add_risktable() +
        ggplot2::facet_wrap(~strata, nrow = 1)
    )
  )
})


test_that("add_risktable() custom stats", {
  expect_error(
    lst_custom_stats <-
      list(sf1, sf2, sf3) %>%
      lapply(
        function(x) {
          ggsurvfit(x) +
            add_risktable(
              risktable_stats =
                c("{n.risk} ({cum.event})",
                  "{round(estimate*100)}% ({round(conf.low*100)}, {round(conf.high*100)})"),
              stats_label = c("At Risk (Cum. Events)", "Survival (95% CI)")
            )
        }
      ),
    NA
  )
  expect_error(lst_custom_stats %>% lapply(function(x) print(x)), NA)

  expect_error(
    lst_custom_stats2 <-
      list(sf1, sf2, sf3) %>%
      lapply(
        function(x) {
          ggsurvfit(x) +
            add_risktable(
              risktable_stats = "{n.risk} ({cum.event})",
              stats_label = list("n.risk" = "No. at Risk")
            )
        }
      ),
    NA
  )
  expect_error(lst_custom_stats2 %>% lapply(function(x) print(x)), NA)

  # only check on mac
  skip_on_ci()
  vdiffr::expect_doppelganger("sf1-risktable-custom-stats-and-label", lst_custom_stats[[1]])
  vdiffr::expect_doppelganger("sf2-risktable-custom-stats-and-label", lst_custom_stats[[2]])
  vdiffr::expect_doppelganger("sf3-risktable-custom-stats-and-label", lst_custom_stats[[3]])

  vdiffr::expect_doppelganger("sf1-risktable-custom-stats-and-label2", lst_custom_stats2[[1]])
  vdiffr::expect_doppelganger("sf2-risktable-custom-stats-and-label2", lst_custom_stats2[[2]])
  vdiffr::expect_doppelganger("sf3-risktable-custom-stats-and-label2", lst_custom_stats2[[3]])
})


test_that("add_risktable() works with Cox models", {
  # runs without error
  strata <- survival::strata
  sf_cox <-
    survival::coxph(Surv(time, status) ~ age + strata(sex), data = df_lung) %>%
    survfit2()
  expect_error(
    (ggsurvfit(sf_cox) + add_risktable()) %>%
      ggsurvfit_build(),
    NA
  )

  # risk table matches with Cox models
  expect_equal(
    sf_cox %>%
      tidy_survfit(times = 0:4 * 10) %>%
      dplyr::select(time, strata,
                    n.risk, n.event, n.censor,
                    cum.event, cum.censor),
    survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
      tidy_survfit(times = 0:4 * 10) %>%
      dplyr::select(time, strata,
                    n.risk, n.event, n.censor,
                    cum.event, cum.censor)
  )

  # not compatible with `add_pvalue()`
  expect_message(
    sf_cox %>%
      ggsurvfit() +
      add_pvalue()
  )
})

test_that("add_risktable() works with ggsurvfit() `start.time` and negative times", {
  expect_error(
    sf_negative_time <-
      survfit(Surv(time - 10, status) ~ 1, df_lung, start.time = -10) %>%
      ggsurvfit() +
      add_risktable(),
    NA
  )

  expect_error(
    sf_start_time <-
      survfit(Surv(time, status) ~ sex, df_lung, start.time = 10) %>%
      ggsurvfit() +
      add_risktable(),
    NA
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("sf-negative_time", sf_negative_time)
  vdiffr::expect_doppelganger("sf-start_time", sf_start_time)
})


test_that("add_risktable() works with multiple survival endpoints (Issue #212)", {
  # Create test data similar to the issue scenario
  set.seed(123)
  n <- 100
  
  # Base survival data
  base_data <- data.frame(
    USUBJID = paste0("SUBJ", 1:n),
    AVAL = rexp(n, 0.1),
    CNSR = rbinom(n, 1, 0.3),
    PARAM = "Overall Survival (years)"
  )
  
  # Stacked data with multiple endpoints (the scenario causing the bug)
  stacked_adtte <- 
    base_data %>%
    dplyr::bind_rows(
      base_data %>%
      dplyr::mutate(
        AVAL = AVAL / (1 + runif(n())),
        PARAM = "Progression-Free Survival (years)"
      )
    )
  
  # Test the exact scenario from Issue #212 that was failing
  expect_error(
    p_issue_212 <- survfit2(Surv_CNSR(AVAL, CNSR) ~ PARAM, data = stacked_adtte) %>%
      ggsurvfit() +
      add_confidence_interval() +
      add_risktable(risktable_group = "strata", risktable_height = 0.25) +
      scale_ggsurvfit(),
    NA
  )
  
  # Ensure the plot can be printed (this would fail before the fix)
  expect_error(print(p_issue_212), NA)
  
  # Test other parameter combinations that could trigger similar issues
  expect_error(
    p_auto_group <- survfit2(Surv_CNSR(AVAL, CNSR) ~ PARAM, data = stacked_adtte) %>%
      ggsurvfit() +
      add_risktable(risktable_group = "auto", risktable_height = 0.25),
    NA
  )
  expect_error(print(p_auto_group), NA)
  
  expect_error(
    p_stats_group <- survfit2(Surv_CNSR(AVAL, CNSR) ~ PARAM, data = stacked_adtte) %>%
      ggsurvfit() +
      add_risktable(risktable_group = "risktable_stats", risktable_height = 0.25),
    NA
  )
  expect_error(print(p_stats_group), NA)
  
  # Test without explicit height (should use calculated height)
  expect_error(
    p_no_height <- survfit2(Surv_CNSR(AVAL, CNSR) ~ PARAM, data = stacked_adtte) %>%
      ggsurvfit() +
      add_risktable(risktable_group = "strata"),
    NA
  )
  expect_error(print(p_no_height), NA)
})


test_that("add_risktable() internal height calculation never returns NULL", {
  # Test the internal function directly to ensure it never returns NULL
  
  # Mock data frame with strata
  df_with_strata <- data.frame(
    time = c(1, 2, 3),
    strata = c("A", "B", "A")
  )
  
  df_without_strata <- data.frame(
    time = c(1, 2, 3),
    other_col = c("X", "Y", "Z")
  )
  
  # Test various combinations
  height1 <- ggsurvfit:::.calculate_risktable_height(
    NULL, "strata", c("n.risk", "cum.event"), df_with_strata
  )
  expect_true(is.numeric(height1) && !is.null(height1) && !is.na(height1))
  
  height2 <- ggsurvfit:::.calculate_risktable_height(
    NULL, "strata", c("n.risk"), df_without_strata
  )
  expect_true(is.numeric(height2) && !is.null(height2) && !is.na(height2))
  
  height3 <- ggsurvfit:::.calculate_risktable_height(
    NULL, "risktable_stats", c("n.risk", "cum.event"), df_with_strata
  )
  expect_true(is.numeric(height3) && !is.null(height3) && !is.na(height3))
  
  height4 <- ggsurvfit:::.calculate_risktable_height(
    NULL, "risktable_stats", c("n.risk"), df_without_strata
  )
  expect_true(is.numeric(height4) && !is.null(height4) && !is.na(height4))
  
  # Test the edge case that was problematic before the fix
  height5 <- ggsurvfit:::.calculate_risktable_height(
    NULL, "auto", c("n.risk"), df_without_strata
  )
  expect_true(is.numeric(height5) && !is.null(height5) && !is.na(height5))
})

