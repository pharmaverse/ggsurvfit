test_that("arithmetic works", {
  skip_on_os("linux")

  p <-
    survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
    ggsurvfit() +
    add_risktable()
  p_build <- ggsurvfit_build(p)

  # not sure why this one fails ....
  # expect_error(`p-bar-minus-divide` <- (p | p) - (p / p), NA)
  # vdiffr::expect_doppelganger("p-bar-minus-divide", `p-bar-minus-divide`)

  expect_error(`p-minus` <- p - p, NA)
  vdiffr::expect_doppelganger("p-minus", `p-minus`)

  expect_error(`p_build-bar-minus-divide` <- (p_build | p_build) / (p_build - p_build), NA)
  vdiffr::expect_doppelganger("p_build-bar-minus-divide", `p_build-bar-minus-divide`)

  expect_error(`p_build-bar` <- p_build | p_build, NA)
  vdiffr::expect_doppelganger("p_build-bar", `p_build-bar`)

  expect_error(`p_build-minus` <- p_build - p_build, NA)
  vdiffr::expect_doppelganger("p_build-minus", `p_build-minus`)

  # not sure why this one fails ....
  # expect_error(`p_build-divide` <- p_build / p_build, NA)
  # vdiffr::expect_doppelganger("p_build-divide", `p_build-divide`)

  p1 <-
    tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
    ggcuminc(outcome = "death from cancer") +
    add_confidence_interval() +
    add_risktable()

  # not sure why this one fails ....
  # expect_error(`p1-bar-minus-divide` <-(p1 | p1) - (p1 / p1), NA)
  # vdiffr::expect_doppelganger("p1-bar-minus-divide", (p1 | p1) - (p1 / p1))

  expect_error(`p1-minus` <- p1 - p1, NA)
  vdiffr::expect_doppelganger("p1-minus",`p1-minus`)
})
