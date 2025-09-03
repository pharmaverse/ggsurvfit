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

  os_data <- df_lung %>% dplyr::mutate(PARAM = "Overall Survival")
  pfs_data <- df_lung %>% dplyr::mutate(time = time * 0.7, PARAM = "Progression-Free Survival")
  combined_data <- dplyr::bind_rows(os_data, pfs_data)


  expect_error(
    p <- survfit2(Surv(time, status) ~ PARAM, data = combined_data) %>%
      ggsurvfit() + add_risktable(),
    NA
  )
  expect_error(print(p), NA)
})


test_that("add_risktable() handles large numbers and long labels without overlapping (Issue #230)", {

  # Large patient cohort with descriptive strata labels
  set.seed(123)  # For reproducible results

  large_cohort_data <- data.frame(
    time = c(
      # Extended Time Since Surgery group - longer survival times
      rexp(800, rate = 0.15),
      # Limited Time Since Surgery group - shorter survival times
      rexp(1200, rate = 0.25)
    ),
    status = c(
      rbinom(800, 1, 0.6),   # 60% event rate for extended group
      rbinom(1200, 1, 0.75)  # 75% event rate for limited group
    ),
    surgery_timing = factor(c(
      rep("Extended Time Since Surgery", 800),
      rep("Limited Time Since Surgery", 1200)
    ))
  )

  # Create survfit object with large numbers
  sf_large_cohort <- survfit2(Surv(time, status) ~ surgery_timing, data = large_cohort_data)

  # Large numbers at time 0: ~800 and ~1200 patients at risk
  expect_error(
    p_issue_230 <- sf_large_cohort %>%
      ggsurvfit() +
      add_risktable(risktable_stats = "n.risk"),
    NA
  )

  expect_error(print(p_issue_230), NA)

  # Test with the format from the user's image: "At risk (censored)"
  expect_error(
    p_issue_230_with_censored <- sf_large_cohort %>%
      ggsurvfit() +
      add_risktable(
        risktable_stats = "{n.risk} ({cum.censor})",
        stats_label = "At risk (censored)"
      ),
    NA
  )

  expect_error(print(p_issue_230_with_censored), NA)

  # Test that the plot actually has large numbers at time 0
  risk_data <- sf_large_cohort %>% tidy_survfit(times = 0)
  expect_true(any(risk_data$n.risk >= 500),
              info = "Should have large patient numbers at baseline")

  # Test with even longer strata names that would definitely cause issues
  very_long_labels_data <- large_cohort_data %>%
    dplyr::mutate(
      surgery_timing = factor(
        surgery_timing,
        levels = c("Extended Time Since Surgery", "Limited Time Since Surgery"),
        labels = c(
          "Extended Time Between Surgery and Treatment Initiation",
          "Limited Time Between Surgery and Treatment Initiation"
        )
      )
    )

  sf_very_long <- survfit2(Surv(time, status) ~ surgery_timing, data = very_long_labels_data)

  expect_error(
    p_very_long_labels <- sf_very_long %>%
      ggsurvfit() +
      add_risktable(risktable_stats = "n.risk"),
    NA
  )

  expect_error(print(p_very_long_labels), NA)

  # Skip visual tests on CI but include them for local testing
  skip_on_ci()
  vdiffr::expect_doppelganger("issue-230-large-numbers", p_issue_230)
  vdiffr::expect_doppelganger("issue-230-with-censored", p_issue_230_with_censored)
  vdiffr::expect_doppelganger("very-long-labels", p_very_long_labels)
})

# Additional test specifically for the overlapping issue
test_that("add_risktable() prevents text overlapping with patchwork::free()", {
  # Create a scenario guaranteed to cause overlapping without the fix
  overlap_data <- data.frame(
    time = rexp(2000, 0.1),  # Very large cohort
    status = rbinom(2000, 1, 0.5),
    group = factor(c(
      rep("Group with extremely long descriptive name that would cause overlap", 1000),
      rep("Another group with very long name causing alignment issues", 1000)
    ))
  )

  sf_overlap <- survfit2(Surv(time, status) ~ group, data = overlap_data)

  # This would definitely cause overlapping without patchwork::free()
  expect_error(
    p_overlap_test <- sf_overlap %>%
      ggsurvfit() +
      add_risktable(risktable_stats = "n.risk") +
      # Force narrow margins to test the alignment fix
      theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")),
    NA
  )

  expect_error(print(p_overlap_test), NA)

  # Test that numbers at time 0 are indeed large (>1000)
  baseline_risk <- sf_overlap %>% tidy_survfit(times = 0)
  expect_true(all(baseline_risk$n.risk >= 900),
              info = "All groups should have large patient numbers")

  skip_on_ci()
  vdiffr::expect_doppelganger("overlap-prevention-test", p_overlap_test)
})
