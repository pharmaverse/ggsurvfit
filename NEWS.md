# ggsurvfit (development version)

* Adding support for multi-state models created with `survfit()`, i.e. competing risks from the survival package. (#83)

* Changed the default of the `ggsurvfit_build(combine_plots=)` argument to `TRUE`.

* Added the `add_quantile(x_value=)` argument that places line segments at the time specified.

* Added the `scale_continuous_reduce_pad()` function that wraps both `ggplot2::scale_x_continuous()` and `ggplot2::scale_y_continuous()` and uses reduced padding (via the `expand=` argument) to both scales. (#82)

* Package now depends on {ggplot2}, meaning that it'll be attached anytime {ggsurvfit} is attached. (#62)

* Converted the gallery vignette to an article. (#75)

* Adding function `add_pvalue()` to place p-values in the figure caption or as a text annotation.

# ggsurvfit 0.1.0

* First release.
