cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)

test_that("tidy_cuminc() works", {
  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>% lapply(tidy_cuminc),
    NA
  )

  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>% lapply(tidy_cuminc, times = 12:14),
    NA
  )

  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>% lapply(tidy_cuminc, times = 1),
    NA
  )

  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>% lapply(tidy_cuminc, times = 0),
    NA
  )

  times <- 0:4 * 10
  df_tidied <-
    cuminc1 %>%
    tidy_cuminc() %>%
    dplyr::filter(outcome %in% "death from cancer") %>%
    .add_tidy_times(times = times) %>%
    .add_cumulative_stats() %>%
    .keep_selected_times(times = times)
  expect_equal(
    df_tidied %>% dplyr::pull(n.risk),
    lapply(times, function(x) sum(tidycmprsk::trial$ttdeath >= x)) %>% unlist()
  )
  expect_equal(
    df_tidied %>% dplyr::pull(cum.censor),
    times %>%
      lapply(function(x) sum(tidycmprsk::trial$death_cr == "censor" &
                               tidycmprsk::trial$ttdeath <= x)) %>% unlist()
  )
  expect_equal(
    df_tidied %>% dplyr::pull(cum.event),
    times %>%
      lapply(function(x) sum(tidycmprsk::trial$death_cr == "death from cancer" &
                               tidycmprsk::trial$ttdeath <= x)) %>% unlist()
  )


  expect_equal(
    df_tidied %>% dplyr::pull(n.censor) %>% `[`(-1),
    seq(1, length(times) - 1) %>%
      lapply(function(x) sum(tidycmprsk::trial$death_cr == "censor" &
                               tidycmprsk::trial$ttdeath > times[x] &
                               tidycmprsk::trial$ttdeath <= times[x+1])) %>% unlist()
  )
  expect_equal(
    df_tidied %>% dplyr::pull(n.event) %>% `[`(-1),
    seq(1, length(times) - 1) %>%
    lapply(function(x) sum(tidycmprsk::trial$death_cr == "death from cancer" &
                             tidycmprsk::trial$ttdeath > times[x] &
                             tidycmprsk::trial$ttdeath <= times[x+1])) %>% unlist()
  )

  # THIS TEST DOES NOT PASS DUE TO A BUG IN tidycmprsk
  # times <- 0:4 * 10
  # cols_to_check <-
  #   c("strata", "time", "estimate", "std.error", "conf.low", "conf.high",
  #     "n.risk", "n.event", "n.censor", "cum.event", "cum.censor")
  # expect_equal(
  #   tidycmprsk::tidy(cuminc1, times = times) %>%
  #     dplyr::filter(outcome %in% "death from cancer") %>%
  #     dplyr::select(dplyr::any_of(cols_to_check)),
  #   cuminc1 %>%
  #     tidy_cuminc() %>%
  #     dplyr::filter(outcome %in% "death from cancer") %>%
  #     .add_tidy_times(times = times) %>%
  #     .add_cumulative_stats() %>%
  #     .keep_selected_times(times = times) %>%
  #     dplyr::select(dplyr::any_of(cols_to_check))
  # )

})

test_that("tidy_cuminc() throws appropriate errors", {
  expect_error(tidy_cuminc(mtcars))
  expect_message(tidy_cuminc(cuminc1, times = -5:5))
})
