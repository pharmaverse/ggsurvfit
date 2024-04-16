test_that("scale_ggsurvfit() works", {
  expect_equal(
    (survfit2(Surv(time, status) ~ sex, df_lung) %>%
       ggsurvfit() +
       scale_ggsurvfit()) %>%
      {suppressWarnings(ggplot2::ggplot_build(.))$layout$panel_params[[1]]$x.range},
    (survfit2(Surv(time, status) ~ sex, df_lung) %>%
       ggsurvfit() +
       scale_x_continuous(expand = c(0.025, 0)) +
       scale_y_continuous(
         limits = c(0 - sqrt(.Machine$double.eps), 1 + sqrt(.Machine$double.eps)),
         expand = c(0.025, 0)
       )) %>%
      {suppressWarnings(ggplot2::ggplot_build(.))$layout$panel_params[[1]]$x.range}
  )

  expect_equal(
    (survfit2(Surv(time, status) ~ sex, df_lung) %>%
       ggsurvfit() +
       scale_ggsurvfit()) %>%
      {suppressWarnings(ggplot2::ggplot_build(.))$layout$panel_params[[1]]$y.range},
    (survfit2(Surv(time, status) ~ sex, df_lung) %>%
       ggsurvfit() +
       scale_x_continuous(expand = c(0.015, 0)) +
       scale_y_continuous(
         expand = c(0.025, 0),
         limits = c(0 - sqrt(.Machine$double.eps), 1 + sqrt(.Machine$double.eps)),
         label = scales::label_percent()
       )) %>%
      {suppressWarnings(ggplot2::ggplot_build(.))$layout$panel_params[[1]]$y.range}
  )
})

test_that("label_percent_imposter() works", {
  probs <- 0:4 * 0.25
  expect_equal(
    label_percent_imposter(probs),
    scales::label_percent()(probs)
  )
})
