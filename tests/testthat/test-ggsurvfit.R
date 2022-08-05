sf1 <- survfit2(Surv(time, status) ~ 1, data = df_lung)
sf2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
sf3 <- survfit2(Surv(time, status) ~ sex + ph.ecog, data = df_lung)

test_that("ggsurvfit() works", {
  expect_error(
    list(sf1, sf2, sf3) %>% lapply(ggsurvfit),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(function(x) ggsurvfit(x, type = "risk")),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(function(x) ggsurvfit(x, type = "cumhaz")),
    NA
  )

  expect_error(
    list(sf1, sf2, sf3) %>% lapply(function(x) ggsurvfit(x, type = function(x) 1 - x)),
    NA
  )

  expect_error(ggsurvfit(mtcars))
})
