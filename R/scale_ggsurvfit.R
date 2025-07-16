#' Apply Scales
#'
#' @description
#' The most common figure created with this package is a survival curve.
#' This scale applies modifications often seen in these figures.
#'
#' - `scale_y_continuous(expand = c(0.025, 0), limits = c(0, 1), label = scales::label_percent())`.
#' - `scale_x_continuous(expand = c(0.015, 0), n.breaks = 8)`
#'
#' *NOTE*: The y-axis limits are only set for survival curves.
#'
#' If you use this function, you **must** include **all** scale specifications
#' that would appear in `scale_x_continuous()` or `scale_y_continuous()`.
#' For example, it's common you'll need to specify the x-axis break points.
#' `scale_ggsurvfit(x_scales=list(breaks=0:9))`.
#'
#' To reset any of the above settings to their ggplot2 default, set the value
#' to `NULL`, e.g. `y_scales = list(limits = NULL)`.
#'
#' @param x_scales a named list of arguments that will be passed to `ggplot2::scale_x_continuous()`.
#' @param y_scales a named list of arguments that will be passed to `ggplot2::scale_y_continuous()`.
#'
#' @return a ggplot2 figure
#' @export
#'
#' @rdname scale_ggsurvfit
#' @examples
#' ggsurvfit <-
#'   survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
#'   ggsurvfit(linewidth = 1) +
#'   add_confidence_interval()
#'
#' # use the function defaults
#' ggsurvfit + scale_ggsurvfit()
#'
#' # specify additional scales
#' ggsurvfit +
#'   scale_ggsurvfit(x_scales = list(breaks = seq(0, 8, by = 2)))
#' @inherit ggsurvfit seealso
scale_ggsurvfit <- function(x_scales = list(), y_scales = list()){
  scale_ggsurvfit_empty_list <- list()
  structure(scale_ggsurvfit_empty_list, x_scales = x_scales, y_scales = y_scales, class = "scale_ggsurvfit")
}


#' @export
ggplot_add.scale_ggsurvfit <- function (object, plot, ...) {
  update_scale_ggsurvfit(plot, object)
}

update_scale_ggsurvfit <- function(p, scale_ggsurvfit_empty_list) {
  # setting default arguments
  x_scale_defaults <- list(expand = c(0.025, 0), n.breaks = 8)
  y_scale_defaults <-
    list(
      expand = c(0.025, 0),
      labels =
        switch(rlang::is_installed("scales", version = "1.1.0"), scales::label_percent()) %||%
        label_percent_imposter
    )
  # set limits of a survival curve
  if (isTRUE(p$data$estimate_type[1] == "survival"))
    y_scale_defaults <-
    c(y_scale_defaults,
      list(limits = c(0 - sqrt(.Machine$double.eps), 1 + sqrt(.Machine$double.eps))))

  # getting user-passed arguments
  x_scales <- attr(scale_ggsurvfit_empty_list, "x_scales")
  y_scales <- attr(scale_ggsurvfit_empty_list, "y_scales")

  # apply scales
  p +
    list(
      rlang::inject(ggplot2::scale_x_continuous(
        !!!utils::modifyList(x = x_scale_defaults, val = x_scales %||% list())
      )),
      rlang::inject(ggplot2::scale_y_continuous(
        !!!utils::modifyList(x = y_scale_defaults, val = y_scales %||% list())
      ))
    )
}

# a poor substitute for `scales::label_percent()`,
# in case users don't have an appropriate version of scales installed
label_percent_imposter <- function(x) {
  paste0(format(round(x * 100, digits = 0), trim = TRUE), "%")
}

