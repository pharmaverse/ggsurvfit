
test_that("The function returns a Surv object", {
  expect_error(surv1 <- with(adtte, Surv_CNSR()), NA)
  expect_error(surv2 <- with(adtte, Surv_CNSR()), NA)
  expect_true(inherits(surv1, "Surv"))
  expect_true(inherits(surv2, "Surv"))
})

test_that("The function is compatible with the survival package", {
  expect_error(survfit(Surv_CNSR() ~ 1, data = adtte), NA)
  expect_error(survfit(Surv_CNSR() ~ SEX, data = adtte), NA)

  expect_error(adtte %>% estimate_KM(formula = Surv_CNSR() ~ 1), NA)
  expect_error(adtte %>% estimate_KM(formula = Surv_CNSR() ~ SEX), NA)

  expect_error(survfit(Surv_CNSR(AVAL, CNSR) ~ 1, data = adtte), NA)
  expect_error(survfit(Surv_CNSR(AVAL, CNSR) ~ SEX, data = adtte), NA)

  expect_error(survival::coxph(Surv_CNSR() ~ SEX, data = adtte), NA)
})


test_that("The results of the estimation match between Surv_CNSR and Surv with inverted censoring", {
  expect_equal(
    with(adtte, Surv_CNSR()),
    with(adtte, Surv(AVAL, 1 - CNSR))
  )

  km1 <- adtte %>% estimate_KM(formula = Surv_CNSR() ~ 1)
  km2 <- adtte %>% estimate_KM()
  km1$call <- km2$call <- NULL
  expect_equal(km1, km2)

  km1 <- adtte %>% estimate_KM(formula = Surv_CNSR() ~ SEX)
  km2 <- adtte %>% estimate_KM(strata = "SEX")
  km1$call <- km2$call <- NULL
  expect_equal(km1, km2)
})


test_that("An error when column name specified through AVAL is not present in the environment", {
  expect_true(!"AVAL" %in% colnames(survival::lung))
  expect_error(survfit(Surv_CNSR() ~ 1, data = survival::lung))
  expect_error(survfit(Surv_CNSR() ~ sex, data = survival::lung))

  adtte[["AVAL"]] <- NULL
  expect_error(survfit(Surv_CNSR() ~ 1, data = adtte))
})

test_that("An error when column name specified through AVAL in the environment is not numeric", {
  adtte[["AVAL"]] <- as.character(adtte[["AVAL"]])
  expect_error(survfit(Surv_CNSR() ~ 1, data = adtte))

  expect_error(survfit(Surv_CDISC(AVAL = time) ~ 1, data = survival::lung %>% dplyr::mutate(AVAL = as.character(time))))
})

test_that("A warning when the column specified through AVAL has negative values", {
  expect_warning(survfit(Surv_CNSR() ~ 1, data = adtte %>% dplyr::mutate(AVAL = AVAL - 10000)))
})

test_that("An error when the column name specified through CNSR is not present in the environment", {
  expect_true(!"CNSR" %in% colnames(survival::lung))
  expect_error(survfit(Surv_CNSR() ~ 1, data = survival::lung %>% dplyr::rename(AVAL = time)))
  expect_error(survfit(Surv_CNSR() ~ sex, data = survival::lung %>% dplyr::rename(AVAL = time)))

  adtte[["CNSR"]] <- NULL
  expect_error(survfit(Surv_CNSR() ~ 1, data = adtte))
})

test_that("An error when the column name specified through CNSR in the environment is not numeric", {
  adtte[["CNSR"]] <- as.character(adtte[["CNSR"]])
  expect_error(survfit(Surv_CNSR() ~ 1, data = adtte))

  expect_error(survfit(Surv_CDISC(AVAL = time) ~ 1, data = survival::lung %>% dplyr::mutate(CNSR = as.character(status))))
})

test_that("An error when the column name specified through CNSR is not coded as 0/1", {
  expect_error(survfit(Surv_CNSR(time, status) ~ 1, data = survival::lung))
})
