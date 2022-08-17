
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggsurvfit

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ddsjoberg/ggsurvfit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ddsjoberg/ggsurvfit/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ddsjoberg/ggsurvfit/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ddsjoberg/ggsurvfit?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggsurvfit)](https://CRAN.R-project.org/package=ggsurvfit)
<!-- badges: end -->

## Introduction

The **ggsurvfit** package eases the creation of time-to-event (aka
survival) endpoint figures with ggplot2. The concise and modular code
creates images that are ready for publication or sharing. Competing
risks cumulative incidence is also supported via `ggcuminc()`.

## Why ggsurvfit?

-   **Use ggplot2 functions:** Each **ggsurvfit** add-on function
    (e.g. `add_confidence_interval()`, `add_risktable()`, etc.) is
    written as a proper ggplot2 ‘geom’, meaning that the package
    functions can be woven with ggplot2 functions seamlessly. You don’t
    need to learn how to style the plot within the ggsurvfit functions:
    you may rely on the suite ggplot2 functions you have already
    learned.

-   **Publishable Legends:** Raw variable names do not appear in the
    figure legend, e.g. `"sex=Female"`.

-   **Limitless Customization:** You can modify the x-axis scales or any
    other plot feature and the risk table will still align with the
    plot.

## Installation

You can install the development version of **ggsurvfit** from
[GitHub](https://github.com/ddsjoberg/ggsurvfit) with:

``` r
# install.packages("devtools")
devtools::install_github("ddsjoberg/ggsurvfit")
```

## Examples

Review the [**figure
gallery**](http://www.danieldsjoberg.com/ggsurvfit/articles/gallery.html)
for many more examples.

``` r
library(ggsurvfit)
library(ggplot2)

survfit2(Surv(time, status) ~ surg, data = df_colon) |>
  # build Kaplan-Meier plot ----------------------------------------------------
  ggsurvfit(size = 1) +
  add_confidence_interval() +
  add_risktable() +
  add_quantile(y_value = 0.6, color = "gray50", size = 0.75) +
  # use ggplot2 functions to style the plot and update the labels --------------
  labs(
    y = "Probability of survival",
    title = "Recurrence by Time Since Surgery",
  ) +
  # reduce padding on edges of figure, and format axes
  scale_y_continuous(label = scales::percent, 
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0.015, 0)) +
  scale_x_continuous(breaks = 0:10, 
                     expand = c(0.02, 0))
```

<img src="man/figures/README-example-1.png" width="100%" />

## `survfit2()` vs `survfit()`

Both functions have identical inputs, so why do we need `survfit2()`?
The `survfit2()` tracks the environment from which the function was
called, resulting in the following benefits.

-   We can reliably remove the raw variable names from the figure
    legend, e.g. `SEX=Female`.
-   P-values can be calculated with `survfit_p()` and added to figures.
-   The items above are often *possible* using `survfit()`. However, by
    utilizing the calling environment we are assured the correct
    elements are found, rather than crossing our fingers that the search
    path contains the needed elements.

## CDISC ADaM ADTTE

The package also includes a gem for those using the [CDISC ADaM
ADTTE](https://www.cdisc.org/standards/foundational/adam/adam-basic-data-structure-bds-time-event-tte-analyses-v1-0)
data model. The event indicator in ADTTE data sets is named `"CNSR"` and
is coded in the opposite way the survival package expects
outcomes—`1 = 'censored'` and `0 = 'event'`. This difference creates an
opportunity for errors to be introduced in an analysis. The package
exports a function called `Surv_CNSR()` to resolve this concern.

The function creates a survival object (e.g. `survival::Surv()`) that
uses CDISC ADaM ADTTE coding conventions and converts the arguments to
the status/event variable convention used in the survival package for
both the event indicator and the time component. The `"AVAL"` and
`"CNSR"` arguments are passed to
`survival::Surv(time = AVAL, event = 1 - CNSR, type = "right", origin = 0)`.
The function can be used in **ggsurvfit** as well as any other package
that uses `survival::Surv()`.

``` r
survfit(Surv_CNSR() ~ 1, adtte)
#> Call: survfit(formula = Surv_CNSR() ~ 1, data = adtte)
#> 
#>         n events median 0.95LCL 0.95UCL
#> [1,] 2199    755    3.2     3.1    3.56
```
