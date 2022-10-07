test_that("grid.draw() works", {
  save_image_png <- function(x) {
    path <- tempfile(fileext = ".png")
    ggplot2::ggsave(filename = path, plot = x)
    path
  }

  expect_error(
    save_image_png(
      survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
        ggsurvfit() +
        add_risktable()
    ),
    NA
  )

  expect_error(
    save_image_png(
      tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
        ggcuminc(outcome = "death from cancer") +
        add_confidence_interval() +
        add_risktable()
    ),
    NA
  )
})
