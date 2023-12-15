sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("survfit2_p() works", {
  expect_type(survfit2_p(sf2, FALSE), "double")
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
  expect_error(survfit2_p(sf2, "aa"))
})

test_that("survfit2_hr() works as expected", {
  expect_type(survfit2_hr(sf2), "double")
  expect_equal(names(survfit2_hr(sf2)), "estimate")
  expect_equal(
    names(survfit2_hr(sf2, TRUE)),
    c("estimate", "ci.low", "ci.high")
  )
  expect_equal(
    survfit2_hr(sf2, TRUE, exponentiate = TRUE),
    exp(survfit2_hr(sf2, TRUE))
  )
})
