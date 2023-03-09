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

  expect_error(
    risktable_overall2 <-
      sf1 %>%
      ggsurvfit() +
      add_risktable(risktable_stats = c("n.risk", "cum.event"),
                    risktable_group = "risktable_stats"),
    NA
  )

  # when weights are present, the risktable Ns should be rounded to nearest integer
  expect_error(
    risktable_with_weights <-
      survfit2(
        formula = Surv(time, status) ~ 1,
        data = df_lung,
        weights = abs(scale(age))
      ) %>%
      ggsurvfit() +
      add_risktable(),
    NA
  )


  skip_on_os("linux")
  vdiffr::expect_doppelganger("sf1-risktable", lst_survfit2_risktable[[1]])
  vdiffr::expect_doppelganger("sf2-risktable", lst_survfit2_risktable[[2]])
  vdiffr::expect_doppelganger("sf3-risktable", lst_survfit2_risktable[[3]])

  vdiffr::expect_doppelganger(
    "sf1-risktable-height",
    sf1 %>% ggsurvfit() + add_risktable(risktable_height = 0.40)
  )
  vdiffr::expect_doppelganger("add_risktable-overall1", risktable_overall1)
  vdiffr::expect_doppelganger("add_risktable-overall2", risktable_overall2)
  vdiffr::expect_doppelganger("add_risktable-weights", risktable_with_weights)
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


  skip_on_os("linux")
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

  skip_on_os("linux")
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
