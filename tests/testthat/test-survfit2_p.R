sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("survfit2_p() works", {
  expect_equal(
    survfit2_p(sf2, prepend_p = FALSE),
    survival::survdiff(Surv(time, status) ~ sex, data = df_lung) %>%
      broom::glance() %>%
      dplyr::pull(p.value) %>%
      format_p()
  )

  expect_equal(
    survfit2_p(sf3, prepend_p = FALSE),
    survival::survdiff(Surv(time, status) ~ sex + ph.ecog, data = df_lung) %>%
      broom::glance() %>%
      dplyr::pull(p.value) %>%
      format_p()
  )

  expect_equal(
    survfit2_p(sf3, prepend_p = FALSE),
    survival::survdiff(Surv(time, status) ~ sex + ph.ecog, data = df_lung) %>%
      broom::glance() %>%
      dplyr::pull(p.value) %>%
      format_p()
  )
})

test_that("survfit2_p() throws error", {
  expect_error(survfit2_p(mtcars))

  expect_snapshot(
    error = TRUE,
    survfit2(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc() +
      add_pvalue()
  )
})

test_that("survfit2_p() works with subset argument", {
  #create survfit object with subset argument
  sf_subset <- survfit2(Surv(time, status) ~ sex, data = df_lung, subset = age >= 75)

  #calculate expectes p val directly with survdiff using the same subset
  expected_p <- survival::survdiff(
    Surv(time, status) ~ sex,
    data = df_lung,
    subset = age >= 75
  ) %>%
    broom::glance() %>%
    dplyr::pull(p.value) %>%
    format_p()

  #testing that survfit2_p respects the subset argument
  expect_equal(
    survfit2_p(sf_subset, prepend_p = FALSE),
    expected_p
  )

  #the subset p-value should be different from full data p-value
  sf_full <- survfit2(Surv(time, status) ~ sex, data = df_lung)

  expect_false(
    survfit2_p(sf_subset, prepend_p = FALSE) == survfit2_p(sf_full, prepend_p = FALSE)
  )

  #testing that subset using filter() gives same result as subset argument
  sf_filtered <- survfit2(Surv(time, status) ~ sex, data = df_lung %>% dplyr::filter(age >= 75))

  expect_equal(
    survfit2_p(sf_subset, prepend_p = FALSE),
    survfit2_p(sf_filtered, prepend_p = FALSE)
  )
})
