test_that("scale_continuous_reduce_pad() works", {
  expect_equal(
    (survfit2(Surv(time, status) ~ sex, df_lung) %>%
       ggsurvfit() +
       scale_continuous_reduce_pad()) %>%
      {ggplot2::ggplot_build(.)$layout$panel_params[[1]]$x.range},
    (survfit2(Surv(time, status) ~ sex, df_lung) %>%
       ggsurvfit() +
       scale_x_continuous(expand = c(0.015, 0)) +
       scale_y_continuous(expand = c(0.025, 0))) %>%
      {ggplot2::ggplot_build(.)$layout$panel_params[[1]]$x.range}
  )

  expect_equal(
    (survfit2(Surv(time, status) ~ sex, df_lung) %>%
      ggsurvfit() +
      scale_continuous_reduce_pad()) %>%
     {ggplot2::ggplot_build(.)$layout$panel_params[[1]]$y.range},
    (survfit2(Surv(time, status) ~ sex, df_lung) %>%
      ggsurvfit() +
      scale_x_continuous(expand = c(0.015, 0)) +
      scale_y_continuous(expand = c(0.025, 0))) %>%
      {ggplot2::ggplot_build(.)$layout$panel_params[[1]]$y.range}
  )
})
