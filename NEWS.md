# ggsurvfit (development version)

* For multi-state models created with `survfit()`, the y-axis label is now "Probability in State". (#205)

* Updated legend position syntax to account for changes in {ggplot2} v3.5.0.

* The `tidy_survfit()` (and subsequently `ggsurvfit()`) now honor the `survfit(start.time)` if specified. (#192)

* We now allow for negative follow-up times in `tidy_survfit()` (and subsequently `ggsurvfit()`). When negative follow-up times are present users should specify `survfit(start.time)` and we print a note to this effect when not set. (#192) 

* As of {survival} v3.6-4, the number censored are now returned as a matrix for multi-state models (i.e. competing risks models). The `tidy_survfit()` function has been updated to account for this new structure and the minimum version of {survival} has been increased to the latest version. (#199)

* Added the "`cloglog`" transformation option to `ggsurvfit(type)` and `tidy_survfit(type)`. (#194)

# ggsurvfit 1.0.0

* By default, a model plot created with `ggsurvfit()` or `ggcuminc()` uses the color aesthetic to plot curves by the stratifying variable(s), and further, `ggcuminc()` uses the linetype aesthetic for plots that contain multiple outcomes (i.e. competing events). We now introduce the global option `"ggsurvfit.switch-color-linetype"` to switch these defaults, giving users more flexibility over the output figures. Furthermore, when the `linetype_aes=` argument is called in a situation when it does not apply, it will be silently ignored (previously, an error message _may_ have been thrown). (#166)

* Slightly increased the padding to the right of the plot when `scale_ggsurvfit()` is called. (#165)

* Now exporting the utility function `ggsurvfit_align_plots()` that is used to ensure risktable and the primary plot align. By exporting this function, users will now be able to construct custom risktable graphics. See `?ggsurvfit_align_plots()` for an example. (#175)

# ggsurvfit 0.3.1

* For transformations in `tidy_survfit()` that change the monotonicity of the curve, the `conf.low` and `conf.high` column names are now switched. (#154)

# ggsurvfit 0.3.0

* Added feature in `add_risktable(risktable_stats=)` to accept glue-like syntax---anything inside curly brackets will be evaluated. Users can now place multiple types of statistics on the same row, including allowing users to style the statistics in any way they like by adding rounding/formatting functions within the curly brackets. Users may now also display estimates and confidence limits in the risk table. (#135)

* Updated `ggsurvfit()`, `tidy_survfit()`, and `survfit2()` to handle `survival::coxph()` models. (#9)

* Updated the default margin size when a risktable is added to a figure. Namely, the primary plot's bottom margin is set to zero, along with each risktable's top and bottom margin. Moreover, the top and bottom margin of the legend is also set to zero.

# ggsurvfit 0.2.1

* All calls to `aes()` have been migrated from `ggplot()` to the individual geoms, e.g. `geom_step()`. This was done because adding the `aes()` call in `ggplot()` led to an error when a later geom is added with user-created data. (#127)

* Delay adding the `conf.low` and `conf.high` to the ggplot `aes()` until `add_confidence_interval()` has been called. Previously, these were being added in the first call to `ggplot(tidy_data, aes(x = time, y = estimate, ymin = conf.low, ymax = conf.high))`. The result was that figures that did _not_ show the confidence interval still created space for the CI in the plot area. This update eliminates that blank space. (#123)

* Migrated the 'scales' package from 'Imports:' to 'Suggests:', i.e. from a strong to a weak dependency. (#120)

* Updated ggplot `size=` argument to `linewidth=` where needed as of ggplot2 v3.4.0 (#131)

# ggsurvfit 0.2.0

### Breaking changes

* Changed the default of the `ggsurvfit_build(combine_plots=)` argument to `TRUE`.

### New features

* Added function `add_pvalue()` to place p-values in the figure caption or as a text annotation.

* Added the `add_quantile(x_value=)` argument that places line segments at the time specified.

* Added the `scale_ggsurvfit()` function that wraps both `ggplot2::scale_x_continuous()` and `ggplot2::scale_y_continuous()` and uses reduced padding (via the `expand=` argument), labels y-axis with percentages (`labels=`), adds additional break points on the x-axis (`n.breaks=8`), and sets the y-axis limits to `c(0, 1)` (`limits=`). (#82)

* Added function `add_legend_title()` that adds a title for the strata in the figure legend. 

* Package now depends on {ggplot2}, meaning that it'll be attached anytime {ggsurvfit} is attached. (#62)

* Added S3 methods `grid.draw.ggsurvfit()` and `grid.draw.ggcuminc()` which in turn allows us to save images from the package directly with `ggplot2::ggsave()` (#107)

* Added support for multi-state models created with `survfit()`, i.e. competing risks from the survival package. (#83)

### Minor improvements and fixes

* Updated the default behavior of `add_risktable(risktable_group='auto')` to minimize the number of risk tables that appear below the figure. (#117) 

* Increased the default font size on the plot and in the risk tables, and added arguments to control font size in the risk table theme. (#103) 

* When using a CDISC ADTTE data frame, the label saved in PARAM/PARAMCD will be used as the default x-axis label in `ggsurvfit()`. (#97)

* When the `survfit(weights=)` argument is utilized, the number at risk, number of observed events, etc. are a non-integer numbers. The counts in the risk table are now rounded to the nearest integer. (#90)

* Converted the gallery vignette to an article. (#75)

* Bug fix when `Surv_CNSR()` is used in conjunction with `ggsurvfit()`. The default x-axis label is incorrectly attributed to a stratifying variable, when present. (#100)

* Fix in `survfit2()` that allows users to pass arguments with non-standard evaluation, i.e. bare column names. (#90)

# ggsurvfit 0.1.0

* First release.
