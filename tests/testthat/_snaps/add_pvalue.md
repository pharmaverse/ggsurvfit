# add_pvalue() throws proper errors

    Code
      tidycmprsk::cuminc(Surv(ttdeath, death_cr) ~ trt, tidycmprsk::trial) %>%
        ggcuminc(outcome = c("death other causes", "death from cancer")) + add_pvalue()
    Condition
      Error in `update_add_pvalue()`:
      ! `add_pvalue()` supports reporting a single competing event p-value and the plot contains "death from cancer" and "death other causes".
      i To place more than one p-value, calculate the p-values with `tidycmprsk::tidy`, and insert them into the plot using ggplot2 functions.

