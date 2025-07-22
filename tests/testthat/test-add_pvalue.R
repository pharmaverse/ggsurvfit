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
})
