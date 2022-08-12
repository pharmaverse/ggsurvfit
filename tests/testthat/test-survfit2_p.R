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
})
