test_that("add_legend_title() works", {
  # title is placed for typical usage
  expect_error(
    sf1 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
      ggsurvfit() +
      add_legend_title(),
    NA
  )
  expect_equal(
    ggplot2::ggplot_build(sf1)$plot$labels$colour,
    "Time from Surgery to Treatment"
  )

  # title is placed with CI as well
  expect_error(
    sf2 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
      ggsurvfit() +
      add_confidence_interval() +
      add_censor_mark() +
      add_legend_title(),
    NA
  )
  expect_equal(
    ggplot2::ggplot_build(sf2)$plot$labels$colour,
    "Time from Surgery to Treatment"
  )
  expect_equal(
    ggplot2::ggplot_build(sf2)$plot$labels$fill,
    "Time from Surgery to Treatment"
  )

  # title is placed with linetype correctly
  expect_error(
    sf3 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
      ggsurvfit(linetype_aes = TRUE) +
      add_confidence_interval() +
      add_censor_mark() +
      add_legend_title(),
    NA
  )
  expect_equal(
    ggplot2::ggplot_build(sf3)$plot$labels$colour,
    "Time from Surgery to Treatment"
  )
  expect_equal(
    ggplot2::ggplot_build(sf3)$plot$labels$fill,
    "Time from Surgery to Treatment"
  )
  expect_equal(
    ggplot2::ggplot_build(sf3)$plot$labels$linetype,
    "Time from Surgery to Treatment"
  )

  # linetype NOT added for ggcuminc with multiple outcomes
  expect_error(
    cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
      ggcuminc(outcome = c("death from cancer", "death other causes")) +
      add_confidence_interval() +
      add_risktable() +
      add_legend_title(),
    NA
  )
  expect_equal(
    ggplot2::ggplot_build(cuminc1)$plot$labels$colour,
    "Chemotherapy Treatment"
  )
  expect_equal(
    ggplot2::ggplot_build(cuminc1)$plot$labels$fill,
    "Chemotherapy Treatment"
  )
  expect_equal(
    ggplot2::ggplot_build(cuminc1)$plot$labels$linetype,
    NULL
  )

  expect_error(
    survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
      ggsurvfit() +
      add_legend_title(title = "MY CUSTOM TITLE"),
    NA
  )

  # when no variable label present, the title is the variable name
  expect_equal(
    ggplot2::ggplot_build(
      survfit2(Surv(mpg, am) ~ cyl, data = mtcars) %>%
        ggsurvfit() +
        add_legend_title()
    )$plot$labels$colour,
    "cyl"
  )
})

test_that("add_legend_title() produces no warnings with ggplot2 4.0.0", {
  # Basic plot should not produce warnings about unknown labels
  expect_silent({
    sf1 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
      ggsurvfit() +
      add_legend_title()
  })

  # Plot with CI should not produce warnings
  expect_silent({
    sf2 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
      ggsurvfit() +
      add_confidence_interval() +
      add_legend_title()
  })

  # Plot with linetype should not produce warnings
  expect_silent({
    sf3 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
      ggsurvfit(linetype_aes = TRUE) +
      add_legend_title()
  })
})

test_that("add_legend_title() sets titles only for mapped aesthetics", {
  # Basic plot should only set colour title (not fill or linetype)
  sf1 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
    ggsurvfit() +
    add_legend_title("Test Title")

  built_plot <- ggplot2::ggplot_build(sf1)
  
  # Check that the correct title was set
  expect_equal(built_plot$plot$labels$colour, "Test Title")

  # Check that fill/linetype titles were NOT set to our custom title
 
  expect_true(is.null(built_plot$plot$labels$fill) || 
              built_plot$plot$labels$fill != "Test Title")
  expect_true(is.null(built_plot$plot$labels$linetype) || 
              built_plot$plot$labels$linetype != "Test Title")

  # Plot with CI should have both colour and fill titles
  sf2 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
    ggsurvfit() +
    add_confidence_interval() +
    add_legend_title("Test Title")

  built_plot2 <- ggplot2::ggplot_build(sf2)
  expect_equal(built_plot2$plot$labels$colour, "Test Title")
  expect_equal(built_plot2$plot$labels$fill, "Test Title")

  # Plot with linetype should have colour and linetype titles
  sf3 <- survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
    ggsurvfit(linetype_aes = TRUE) +
    add_legend_title("Test Title")

  built_plot3 <- ggplot2::ggplot_build(sf3)
  expect_equal(built_plot3$plot$labels$colour, "Test Title")
  expect_equal(built_plot3$plot$labels$linetype, "Test Title")
})

