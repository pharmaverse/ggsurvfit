# survfit2_p() throws error

    Code
      survfit2(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>% ggcuminc() +
        add_pvalue()
    Message
      Plotting outcome "death from cancer".
    Condition
      Error in `survfit2_p()`:
      ! The `survfit2_p()` does not support multi-state models.

