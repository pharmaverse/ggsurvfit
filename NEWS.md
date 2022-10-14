# ggsurvfit (development version)

* Updated the default behavior of `add_risktable(risktable_group='auto')` to minimize the number of risk tables that appear below the figure. (#117) 

* Added S3 methods `grid.draw.ggsurvfit()` and `grid.draw.ggcuminc()` which in turn allows us to save images from the package directly with `ggplot2::ggsave()` (#107)

* Increased the default font size on the plot and in the risk tables, and added arguments to control font size in the risk table theme. (#103) 

* When using a CDISC ADTTE data frame, the label saved in PARAM/PARAMCD will be used as the default x-axis label in `ggsurvfit()`. (#97)

* Bug fix when `Surv_CNSR()` is used in conjunction with `ggsurvfit()`. The default x-axis label is incorrectly attributed to a stratifying variable, when present. (#100)

* Fix in `survfit2()` that allows users to pass arguments with non-standard evaluation, i.e. bare column names. (#90)

* When the `survfit(weights=)` argument is utilized the number at risk, number of observed events, etc. are a non-integer numbers. The counts in the risk table are now rounded to the nearest integer. (#90)

* Adding ability to report comparative p-values from cumulative incidence plots created with `tidycmprsk::cuminc()` (#84)

* Adding support for multi-state models created with `survfit()`, i.e. competing risks from the survival package. (#83)

* Changed the default of the `ggsurvfit_build(combine_plots=)` argument to `TRUE`.

* Added the `add_quantile(x_value=)` argument that places line segments at the time specified.

* Added the `scale_ggsurvfit()` function that wraps both `ggplot2::scale_x_continuous()` and `ggplot2::scale_y_continuous()` and uses reduced padding (via the `expand=` argument), labels y-axis with percentages (`labels=`), adds additional break points on the x-axis (`n.breaks=8`), and sets the y-axis limits to `c(0, 1)` (`limits=`). (#82)

* Package now depends on {ggplot2}, meaning that it'll be attached anytime {ggsurvfit} is attached. (#62)

* Converted the gallery vignette to an article. (#75)

* Adding function `add_pvalue()` to place p-values in the figure caption or as a text annotation.

# ggsurvfit 0.1.0

* First release.
