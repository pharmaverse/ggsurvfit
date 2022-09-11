sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("tidy_survfit() works with survfit2()", {
  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, type = "risk"),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, type = "cumhaz"),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, type = function(x) 1 - x),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, times = 12:14),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, times = 1),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, times = 0),
    NA
  )

  # checking calculation of n.risk at timepoints
  times <- 0:4 * 10
  expect_equal(
    sf1 %>% tidy_survfit(times = times) %>% dplyr::pull(n.risk),
    lapply(times, function(x) sum(df_lung$time >= x)) %>% unlist()
  )

  df_nrisk_check <- sf2 %>% tidy_survfit(times = times) %>% dplyr::select(strata, time, n.risk)
  expect_equal(
    df_nrisk_check %>% dplyr::filter(strata == "Female") %>% dplyr::pull(n.risk),
    lapply(times, function(x) sum(df_lung$time >= x & df_lung$sex == "Female")) %>% unlist()
  )
  expect_equal(
    df_nrisk_check %>% dplyr::filter(strata == "Male") %>% dplyr::pull(n.risk),
    lapply(times, function(x) sum(df_lung$time >= x & df_lung$sex == "Male")) %>% unlist()
  )

  # checking calculation of event counts at timepoints
  expect_equal(
    sf1 %>% tidy_survfit(times = times) %>% dplyr::pull(cum.event),
    lapply(times, function(x) sum(df_lung$time <= x & df_lung$status == 2)) %>% unlist()
  )

  df_event_check <- sf2 %>% tidy_survfit(times = times) %>% dplyr::select(strata, time, cum.event)
  expect_equal(
    df_event_check %>% dplyr::filter(strata == "Female") %>% dplyr::pull(cum.event),
    lapply(times, function(x) sum(df_lung$time <= x & df_lung$status == 2 & df_lung$sex == "Female")) %>% unlist()
  )
  expect_equal(
    df_event_check %>% dplyr::filter(strata == "Male") %>% dplyr::pull(cum.event),
    lapply(times, function(x) sum(df_lung$time <= x & df_lung$status == 2 & df_lung$sex == "Male")) %>% unlist()
  )

  expect_error(sf2 %>% tidy_survfit(type = mtcars))
})

test_that("tidy_survfit() throws appropriate errors", {
  expect_error(tidy_survfit(mtcars))
  expect_error(tidy_survfit(sf1, times = -5:5))
  expect_error(tidy_survfit(sf1, type = "not_a_type"))
})

test_that("tidy_survfit() works with survfit()", {
  sf1 <- survfit(Surv(time, status) ~ 1, data = df_lung)
  sf2 <- survfit(Surv(time, status) ~ sex, data = df_lung)
  sf3 <- survfit(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, type = "risk"),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, type = "cumhaz"),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, type = function(x) 1 - x),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, times = 12:14),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, times = 1),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(tidy_survfit, times = 0),
    NA
  )
})


test_that("tidy_survfit() works with multi-state models", {
  sfms1 <- survfit2(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
  sfms2 <- survfit2(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
  sfms3 <- survfit2(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)

  cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
  cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
  cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)

  expect_equal(
    sfms1 %>%
      tidy_survfit() %>%
      dplyr::select(dplyr::any_of(c("time", "outcome", "strata", "estimate"))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c("strata", "outcome")), ~as.character(.) %>% trimws())) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(c("time", "outcome", "strata")))),
    cuminc1 %>%
      tidy_cuminc() %>%
      dplyr::select(dplyr::any_of(c("time", "outcome", "strata", "estimate"))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c("strata", "outcome")), ~as.character(.) %>% trimws())) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(c("time", "outcome", "strata"))))
  )

  expect_equal(
    sfms2 %>%
      tidy_survfit() %>%
      dplyr::select(dplyr::any_of(c("time", "outcome", "strata", "estimate"))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c("strata", "outcome")), ~as.character(.) %>% trimws())) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(c("time", "outcome", "strata")))),
    cuminc2 %>%
      tidy_cuminc() %>%
      dplyr::select(dplyr::any_of(c("time", "outcome", "strata", "estimate"))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c("strata", "outcome")), ~as.character(.) %>% trimws())) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(c("time", "outcome", "strata"))))
  )

  expect_equal(
    sfms3 %>%
      tidy_survfit() %>%
      dplyr::select(dplyr::any_of(c("time", "outcome", "strata", "estimate"))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c("strata", "outcome")), ~as.character(.) %>% trimws())) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(c("time", "outcome", "strata")))),
    cuminc3 %>%
      tidy_cuminc() %>%
      dplyr::select(dplyr::any_of(c("time", "outcome", "strata", "estimate"))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c("strata", "outcome")), ~as.character(.) %>% trimws())) %>%
      dplyr::arrange(dplyr::across(dplyr::any_of(c("time", "outcome", "strata"))))
  )

})


