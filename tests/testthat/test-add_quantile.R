sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("add_quantile() works with ggsurvfit()", {
  expect_error(
    lst_survfit2_quantile <-
      list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_quantile()),
    NA
  )
  vdiffr::expect_doppelganger("sf1-quantile", lst_survfit2_quantile[[1]])
  vdiffr::expect_doppelganger("sf2-quantile", lst_survfit2_quantile[[2]])
  vdiffr::expect_doppelganger("sf3-quantile", lst_survfit2_quantile[[3]])

  expect_error(
    list(sf1, sf2, sf3) %>%
      lapply(function(x) (ggsurvfit(x) + add_quantile()) %>% print()),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>%
      lapply(function(x) (ggsurvfit(x, type = function(x) x) + add_quantile()) %>% print()),
    NA
  )

  expect_equal(
    sf1 %>%
      tidy_survfit() %>%
      dplyr::select(x = time, y = estimate) %>%
      quantile_km_in_stat(y_value = 0.5) %>%
      dplyr::pull(x) %>%
      `[`(1),
    quantile(sf1, probs = 0.5, conf.int = FALSE) %>% as.numeric()
  )

  expect_equal(
    sf2 %>%
      tidy_survfit() %>%
      dplyr::select(x = time, y = estimate, group = strata) %>%
      quantile_km_in_stat(y_value = 0.5) %>%
      dplyr::pull(x) %>%
      setdiff(0),
    quantile(sf2, probs = 0.5, conf.int = FALSE) %>% as.numeric()
  )

  # a single quantile line is shown, because only one group reached median survival
  sf2_colon <- survfit2(Surv(time, status) ~ surg, data = df_colon)
  expect_equal(
    sf2_colon %>%
      tidy_survfit() %>%
      dplyr::select(x = time, y = estimate, group = strata) %>%
      quantile_km_in_stat(y_value = 0.5) %>%
      dplyr::pull(x) %>%
      setdiff(0),
    quantile(sf2_colon, probs = 0.5, conf.int = FALSE) %>%
      as.numeric() %>%
      na.omit(),
    ignore_attr = TRUE
  )
  vdiffr::expect_doppelganger(
    "sf2_colon-quantile",
    sf2_colon %>%
      ggsurvfit() +
      add_quantile()
  )

  # no lines added, because 20% not reached
  vdiffr::expect_doppelganger(
    "sf2_colon-quantile-no-line",
    sf2_colon %>%
      ggsurvfit() +
      add_quantile(y_value = 0.2)
  )

  # testing that both increasing and decreasing function work
  vdiffr::expect_doppelganger(
    "sf-mtcars-decreasing",
    survfit2(Surv(mpg, am) ~ cyl, mtcars %>% dplyr::filter(cyl %in% c(4, 6))) %>%
      ggsurvfit() +
      add_quantile()
  )
  vdiffr::expect_doppelganger(
    "sf-mtcars-increasing",
    survfit2(Surv(mpg, am) ~ cyl, mtcars %>% dplyr::filter(cyl %in% c(4, 6))) %>%
      ggsurvfit(type = "risk") +
      add_quantile()
  )


  vdiffr::expect_doppelganger(
    "sf-mtcars-decreasing-many-quantiles",
    survfit2(Surv(mpg, am) ~ cyl, mtcars) %>%
      ggsurvfit() +
      add_quantile(y_value = 0.75, linetype = "dotted") +
      add_quantile(y_value = 0.25)
  )
  vdiffr::expect_doppelganger(
    "sf-mtcars-increasing-many-quantiles",
    survfit2(Surv(mpg, am) ~ cyl, mtcars) %>%
      ggsurvfit(type = "risk") +
      add_quantile(y_value = 0.75, linetype = "dotted") +
      add_quantile(y_value = 0.25)
  )


  vdiffr::expect_doppelganger(
    "sf-mtcars-timing",
    survfit2(Surv(mpg, am) ~ 1, mtcars %>% dplyr::filter(cyl == 4)) %>%
      ggsurvfit() +
      add_quantile(y_value = 0.75) + # previous obs is a censor
      add_quantile(y_value = 0.5) # previous obs is an event
  )

})

test_that("add_quantile() errors with ggsurvfit()", {
  expect_warning(
    (mtcars %>%
       ggplot2::ggplot(ggplot2::aes(y = mpg, x = hp)) +
       add_quantile()) %>%
      print()
  )
})




cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)


test_that("add_quantile() works with ggcuminc()", {
  expect_error(
    lst_cuminc_quantile <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x) + add_quantile(y_value = 0.2)),
    NA
  )

  vdiffr::expect_doppelganger("cuminc1-quantile", lst_cuminc_quantile[[1]])
  vdiffr::expect_doppelganger("cuminc2-quantile", lst_cuminc_quantile[[2]])
  vdiffr::expect_doppelganger("cuminc3-quantile", lst_cuminc_quantile[[3]])


  expect_error(
    lst_cuminc_quantile_outcomes <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x, outcome = c("death from cancer", "death other causes")) + add_quantile(y_value = 0.2)),
    NA
  )

  vdiffr::expect_doppelganger("cuminc1-quantile-all-outcomes", lst_cuminc_quantile_outcomes[[1]])
  vdiffr::expect_doppelganger("cuminc2-quantile-all-outcomes", lst_cuminc_quantile_outcomes[[2]])
  vdiffr::expect_doppelganger("cuminc3-quantile-all-outcomes", lst_cuminc_quantile_outcomes[[3]])
})

