cuminc1 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
cuminc2 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
cuminc3 <- tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)

sf_cuminc1 <- survfit2(Surv(ttdeath, death_cr) ~ 1, data = tidycmprsk::trial)
sf_cuminc2 <- survfit2(Surv(ttdeath, death_cr) ~ trt, data = tidycmprsk::trial)
sf_cuminc3 <- survfit2(Surv(ttdeath, death_cr) ~ trt + grade, data = tidycmprsk::trial)

test_that("ggcuminc() works", {
  expect_error(
    lst_ggcuminc_sf <- list(sf_cuminc1, sf_cuminc2, sf_cuminc3) %>% lapply(ggcuminc),
    NA
  )

  expect_error(
    lst_ggcuminc <- list(cuminc1, cuminc2, cuminc3) %>% lapply(ggcuminc),
    NA
  )

  expect_error(ggcuminc(mtcars))
  expect_error(ggcuminc(cuminc1, outcome = "not an outcome"))

  # the linetype_aes argument is silently ignored here
  expect_error(
    ggcuminc(cuminc1,
             outcome = c("death from cancer", "death other causes"),
             linetype_aes = TRUE),
    NA
  )
  expect_error(
    ggcuminc(cuminc1, outcome = c("death from cancer", "death other causes")),
    NA
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("sf_cuminc1-ggcuminc", lst_ggcuminc_sf[[1]])
  vdiffr::expect_doppelganger("sf_cuminc2-ggcuminc", lst_ggcuminc_sf[[2]])
  vdiffr::expect_doppelganger("sf_cuminc3-ggcuminc", lst_ggcuminc_sf[[3]])

  vdiffr::expect_doppelganger("cuminc1-ggcuminc", lst_ggcuminc[[1]])
  vdiffr::expect_doppelganger("cuminc2-ggcuminc", lst_ggcuminc[[2]])
  vdiffr::expect_doppelganger("cuminc3-ggcuminc", lst_ggcuminc[[3]])

})

test_that("ggcuminc() works with multiple outcomes", {
  expect_error(
    lst_ggcuminc_outcomes <-
      list(cuminc1, cuminc2, cuminc3) %>%
      lapply(ggcuminc, outcome = c("death from cancer", "death other causes")),
    NA
  )


  expect_error(
    lst_ggcuminc_outcomes_sf <-
      list(sf_cuminc1, sf_cuminc2, sf_cuminc3) %>%
      lapply(ggcuminc, outcome = c("death from cancer", "death other causes")),
    NA
  )

  skip_on_ci()
  vdiffr::expect_doppelganger("cuminc1-ggcuminc-all-outcomes", lst_ggcuminc_outcomes[[1]])
  vdiffr::expect_doppelganger("cuminc2-ggcuminc-all-outcomes", lst_ggcuminc_outcomes[[2]])
  vdiffr::expect_doppelganger("cuminc3-ggcuminc-all-outcomes", lst_ggcuminc_outcomes[[3]])
  vdiffr::expect_doppelganger("cuminc1-ggcuminc_sf-all-outcomes", lst_ggcuminc_outcomes_sf[[1]])
  vdiffr::expect_doppelganger("cuminc2-ggcuminc_sf-all-outcomes", lst_ggcuminc_outcomes_sf[[2]])
  vdiffr::expect_doppelganger("cuminc3-ggcuminc_sf-all-outcomes", lst_ggcuminc_outcomes_sf[[3]])
})

test_that("ggcuminc() axis label correct with multi-state model ", {
  d1 <- subset(survival::colon, etype==1)
  d2 <- subset(survival::colon, etype==2)
  cdata <- survival::tmerge(subset(d1,,c(id, rx, extent, node4)), d2, id = id,
                            death = event(time, status))
  cdata <- survival::tmerge(cdata, d1, id = id, recur=event(time, status))

  # a death and recurrence on the same day is counted as a recurrence
  cdata$state <- with(cdata, factor(ifelse(recur==1,1, 2*death),  0:2,
                                    c("censor", "recur", "death")))
  cdata$trt <- 1*(cdata$rx=="Lev+5FU") # lump the Obs and Lev arms together

  cfit <- survfit(Surv(tstart, tstop, state) ~ trt, cdata, id=id)

  expect_equal(
    tidy_survfit(cfit, times = 1000)$estimate_type_label %>% unique(),
    "Probability in State"
  )
})

test_that("ggcuminc() throws error with survfit.coxphms objects", {
  skip_if_not_installed("survival")
  
  d1 <- subset(survival::colon, etype==1)
  d2 <- subset(survival::colon, etype==2)
  cdata <- survival::tmerge(subset(d1,,c(id, rx, extent, node4)), d2, id = id,
                            death = event(time, status))
  cdata <- survival::tmerge(cdata, d1, id = id, recur=event(time, status))
  
  cdata$state <- with(cdata, factor(ifelse(recur==1,1, 2*death),  0:2,
                                    c("censor", "recur", "death")))
  cdata$trt <- 1*(cdata$rx=="Lev+5FU")
  
  # Create a Cox model with multi-state data 
  cox_model <- survival::coxph(
    Surv(tstart, tstop, state) ~ trt, 
    data = cdata, 
    id = id
  )

  # Create newdata for survfit 
  newdata <- data.frame(trt = c(0, 1))
  
  # Create survfit object from Cox model 
  survfit_coxphms_obj <- survival::survfit(cox_model, newdata = newdata)
  
  # Test that the specific error message is thrown
  expect_error(
    ggcuminc(survfit_coxphms_obj),
    "Object passed in argument.*must be class.*tidycuminc.*or.*survfitms.*survfitcoxms.*not supported"
  )
})
