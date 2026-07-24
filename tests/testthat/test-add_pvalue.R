test_that("add_pvalue() works", {
  expect_error(
    tbl_p1 <-
      survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit() +
      add_pvalue(),
    NA
  )

  expect_error(
    tbl_p2 <-
      survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit() +
      add_pvalue("annotation", size = 7),
    NA
  )

  expect_error(
    tbl_p3 <-
      survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit(typ = "risk") +
      add_pvalue("annotation"),
    NA
  )

  # no p-value output when created with survfit()
  expect_error(
    tbl_p4 <-
      survfit(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit(typ = "risk") +
      add_pvalue(),
    NA
  )

  # no error with ggcuminc()
  expect_error(
    pvalue_cuminc <-
      tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc(outcome = "death from cancer") +
      add_confidence_interval() +
      add_risktable() +
      add_pvalue(),
    NA
  )

 # ensure `add_pvalue()` works with all events
  expect_no_message(
    pvalue_cuminc1 <-
      tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc(outcome = "death from cancer") +
      add_pvalue()
  )

  # Verify p-value is added to the plot
  expect_true(grepl("p", pvalue_cuminc1$labels$caption %||% ""))

  expect_no_message(
    pvalue_cuminc_other<-
      tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc(outcome = "death other causes") +
      add_pvalue()
  )

  # Verify p-value is added to the plot
  expect_true(grepl("p", pvalue_cuminc_other$labels$caption %||% ""))

  skip_on_ci()
  vdiffr::expect_doppelganger("sf2-pvalue-caption", tbl_p1)
  vdiffr::expect_doppelganger("sf2-pvalue-annotation", tbl_p2)
  vdiffr::expect_doppelganger("sf2-pvalue-annotation-risk", tbl_p3)
  vdiffr::expect_doppelganger("sf2-pvalue-with_survfit", tbl_p4)
  vdiffr::expect_doppelganger("cuminc2-pvalue", pvalue_cuminc)
})


test_that("add_pvalue() matches the p-value to the plotted outcome by name (#277)", {
  # relevel `death_cr` so the factor ordering (used by `tidy()`) differs from the
  # failcode ordering (used by `glance()` p.value_* columns)
  trial_relevel <- tidycmprsk::trial
  trial_relevel$death_cr <-
    factor(
      as.character(trial_relevel$death_cr),
      levels = c("censor", "death other causes", "death from cancer")
    )

  cuminc_fit <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, trial_relevel)
  glance_df <- tidycmprsk::glance(cuminc_fit)

  # expected p-values keyed by outcome name, matched from glance() by name
  n_outcomes <- sum(grepl("^outcome_[0-9]+$", names(glance_df)))
  expected_p <- vapply(
    seq_len(n_outcomes),
    function(i) glance_df[[paste0("p.value_", i)]],
    numeric(1L)
  )
  names(expected_p) <- vapply(
    seq_len(n_outcomes),
    function(i) glance_df[[paste0("outcome_", i)]],
    character(1L)
  )

  # sanity check: tidy() and glance() orderings actually differ for this fit,
  # otherwise the regression test would not exercise the bug
  expect_false(
    identical(
      unique(tidycmprsk::tidy(cuminc_fit)[["outcome"]]),
      unname(names(expected_p))
    )
  )

  reported_p <- vapply(
    names(expected_p),
    function(outcome) {
      caption <-
        (ggcuminc(cuminc_fit, outcome = outcome) +
           add_pvalue(pvalue_fun = function(x) as.character(x), prepend_p = FALSE))$labels$caption
      as.numeric(caption)
    },
    numeric(1L)
  )

  # each plotted outcome reports its own Gray-test p-value
  expect_equal(reported_p, expected_p)

  # and the two outcomes' p-values are not swapped
  expect_false(isTRUE(all.equal(
    reported_p[["death from cancer"]],
    expected_p[["death other causes"]]
  )))
})


test_that("add_pvalue() throws proper errors", {
  expect_error(
    (survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit() +
      add_pvalue(caption = letters)) %>%
      ggsurvfit_build()
  )
  expect_error(
    (survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit() +
      add_pvalue(pvalue_fun = letters)) %>%
      ggsurvfit_build()
  )
  expect_error(
    (survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit() +
      add_pvalue(prepend_p = letters)) %>%
      ggsurvfit_build()
  )

  expect_snapshot(
    error = TRUE,
    tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc(outcome = c("death other causes", "death from cancer")) +
      add_pvalue()
  )
})
