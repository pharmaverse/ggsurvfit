sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("add_quantile() works with ggsurvfit()", {
  expect_error(
    list(sf1, sf2, sf3) %>%
      lapply(function(x) ggsurvfit(x) + add_quantile()),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>%
      lapply(function(x) (ggsurvfit(x) + add_quantile()) %>% print()),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>%
      lapply(function(x) (ggsurvfit(x, type = function(x) x) + add_quantile()) %>% print()),
    NA
  )
})

test_that("add_quantile() errors with ggsurvfit()", {
  expect_warning(
    (mtcars %>%
       ggplot2::ggplot(ggplot2::aes(y = mpg, x = hp)) +
       add_quantile()) %>%
      print()
  )
})




cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)


test_that("add_quantile() works with ggcuminc()", {
  expect_error(
    list(cuminc1, cuminc2, cuminc3) %>%
      lapply(function(x) ggcuminc(x) + add_quantile()),
    NA
  )
})

