
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggsurvfit

<!-- badges: start -->

[![R-CMD-check](https://github.com/ddsjoberg/ggsurvfit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ddsjoberg/ggsurvfit/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggsurvfit)](https://CRAN.R-project.org/package=ggsurvfit)
[![Codecov test
coverage](https://codecov.io/gh/ddsjoberg/ggsurvfit/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ddsjoberg/ggsurvfit?branch=main)
<!-- badges: end -->

## Introduction

The {ggsurvfit} package eases the creation of time-to-event (aka
survival) endpoint figures with ggplot2. The concise and modular code
creates images that are ready for publication or sharing.

## Why ggsurvfit?

-   **Publishable Legends** Raw variable names do not appear in the
    figure legend, e.g. `"sex=Female"`.

-   **Use ggplot2 functions** Each {ggsurvfit} add-on function
    (e.g. `add_confidence_interval()`, `add_risktable()`, etc.) is
    written as a proper ggplot2 ‘geom’, meaning that the package
    functions can be woven with ggplot2 functions seamlessly.

-   **Limitless Customization** You can modify the x-axis scales or any
    other plot feature and the risk table will still align with the
    plot.

## Installation

You can install the development version of ggsurvfit from
[GitHub](https://github.com/ddsjoberg/ggsurvfit) with:

``` r
# install.packages("devtools")
devtools::install_github("ddsjoberg/ggsurvfit")
```

## Examples

Let’s begin with an example illustrating a common Kaplan-Meier survival
curve.

It’s recommended to use the `survfit2()` function with this package,
rather than `survival::survfit()` to ensure all quantities are able to
be calculated from any environment the functions are called.

``` r
library(ggsurvfit)
library(ggplot2)

p <- 
  survfit2(Surv(time, status) ~ sex, data = df_lung) |>
  ggsurvfit(size = 1) +
  add_censor_mark() +
  add_confidence_interval() +
  add_risktable() +
  add_quantile(color = "gray50", size = 1)
p
```

<img src="man/figures/README-example-1.png" width="100%" />

<hr>

Because each of the functions are written as proper ggplot geoms, you
can add any ggplot function to modify the figure.

``` r
p +
  labs(
    y = "Probability of survival",
    x = "Months since treatment",
    title = "Kaplan-Meier Estimate of Survival by Sex",
    # remove the fill and color legend labels (Sex appears in title)
    fill = NULL, color = NULL
  ) +
  # reduce padding on edges of figure, and format axes
  scale_y_continuous(label = scales::percent, expand = c(0.01, 0)) +
  scale_x_continuous(breaks = 0:5*6, expand = c(0.02, 0))
```

<img src="man/figures/README-example-styled-1.png" width="100%" />

<hr>

The package also supports unstratified models and you can add multiple
quantile guidelines.

``` r
survfit2(Surv(time, status) ~ 1, data = df_lung) |>
  ggsurvfit(size = 1) +
  add_quantile(linetype = 3, size = 1) +
  add_quantile(y_value = 0.25, linetype = 2, size = 1) +
  add_confidence_interval() +
  add_risktable()
```

<img src="man/figures/README-example-unstrat-1.png" width="100%" />

<hr>

You can even facet…but you *cannot* facet with the risktable, however.

``` r
survfit2(Surv(time, status) ~ sex, data = df_lung) |>
  ggsurvfit(size = 1) +
  add_censor_mark(shape = 4) +
  add_quantile(linetype = 3, size = 1, color = "gray50") +
  add_confidence_interval() +
  ggplot2::facet_grid(~strata)
```

<img src="man/figures/README-example-facet-1.png" width="100%" />

<hr>

The package works seamlessly with other ggplot extension package, such
as {gghighlight}.

``` r
survfit2(Surv(time, status) ~ ph.ecog, data = df_lung) |> 
  ggsurvfit(size = 1) +
  ggplot2::labs(color = "ECOG") +
  gghighlight::gghighlight(strata == "Asymptomatic", calculate_per_facet = TRUE)
```

<img src="man/figures/README-example-gghighlight-1.png" width="100%" />

<hr>

The package also plots cumulative incidence estimates in the presence of
competing events.

``` r
library(tidycmprsk)

cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
  ggcuminc(outcome = "death from cancer", size = 1) +
  add_confidence_interval() +
  add_quantile(y_value = 0.20, size = 1, color = "gray50") +
  add_risktable() +
  labs(x = "Months Since Treatment") +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = scales::percent, expand = c(0.02, 0)) +
  scale_x_continuous(breaks = 0:4 * 6, expand = c(0.02, 0))
```

<img src="man/figures/README-example-cuminc-1.png" width="100%" />
