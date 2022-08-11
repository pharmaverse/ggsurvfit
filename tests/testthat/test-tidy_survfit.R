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

  times <- 0:3 * 10
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


