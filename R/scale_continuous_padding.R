#' Reduce Scale Padding
#'
#' @description
#' I have a hard time remembering the syntax to reduce horizontal and vertical
#' padding on a ggplot2 figure. Hence, a function with default reduced padding.
#'
#' If you use this function, you **must** include all scale specifications
#' that would appear in `scale_x_continuous()` or `scale_y_continuous()`.
#'
#' @param x_scales a named list of arguments that will be passed to `ggplot2::scale_x_continuous()`.
#' @param y_scales a named list of arguments that will be passed to `ggplot2::scale_y_continuous()`.
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' ggsurvfit <-
#'   survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
#'   ggsurvfit(size = 1) +
#'   add_confidence_interval()
#'
#' # use the function defaults
#' ggsurvfit + scale_continuous_reduce_pad()
#'
#' # specify additional scales
#' ggsurvfit +
#'   scale_continuous_reduce_pad(
#'     x_scales = list(breaks = 0:9),
#'     y_scales = list(label = scales::percent, limits = c(0, 1))
#'   )
scale_continuous_reduce_pad <- function(x_scales = list(), y_scales = list()) {
  list(
    rlang::inject(ggplot2::scale_x_continuous(!!!utils::modifyList(x = list(expand = c(0.015, 0)), val = x_scales))),
    rlang::inject(ggplot2::scale_y_continuous(!!!utils::modifyList(x = list(expand = c(0.025, 0)), val = y_scales)))
  )
}
