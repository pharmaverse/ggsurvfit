test_that("add_pvalue() works", {
  expect_error(
    tbl_p1 <-
      survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit() +
      add_pvalue(),
    NA
  )
  vdiffr::expect_doppelganger("sf2-pvalue-caption", tbl_p1)

  expect_error(
    tbl_p2 <-
      survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit() +
      add_pvalue("annotation", size = 7),
    NA
  )
  vdiffr::expect_doppelganger("sf2-pvalue-annotation", tbl_p2)

  expect_error(
    tbl_p3 <-
      survfit2(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit(typ = "risk") +
      add_pvalue("annotation"),
    NA
  )
  vdiffr::expect_doppelganger("sf2-pvalue-annotation-risk", tbl_p3)

  # no p-value output when created with survfit()
  expect_error(
    tbl_p4 <-
      survfit(Surv(time, status) ~ surg, df_colon) %>%
      ggsurvfit(typ = "risk") +
      add_pvalue(),
    NA
  )
  vdiffr::expect_doppelganger("sf2-pvalue-with_survfit", tbl_p4)

  # no error with ggcuminc()
  expect_error(
    tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc(outcome = "death from cancer") +
      add_confidence_interval() +
      add_risktable() +
      add_pvalue(),
    NA
  )
})
