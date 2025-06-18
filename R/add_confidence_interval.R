#' Add Confidence Interval
#'
#' Add a confidence interval represented by either a ribbon or lines.
#'
#' @param type string indicating the type of confidence interval to draw.
#' Must be one of `c("ribbon", "lines")`
#' @param ... arguments pass to geom.
#' - `type = 'ribbon'`: Defaults are `ggplot2::geom_ribbon(alpha = 0.2, color = NA, ...)`
#' - `type = 'lines'`: Defaults are `ggplot2::geom_step(linetype = "dashed", na.rm = TRUE, ...)`
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_confidence_interval() +
#'   scale_ggsurvfit()
#'
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_confidence_interval(type = "lines") +
#'   scale_ggsurvfit()
#' @inherit ggsurvfit seealso
add_confidence_interval <- function(type = c("ribbon", "lines"), ...) {
  add_confidence_interval_empty_list <- list()
  structure(add_confidence_interval_empty_list,
            type = match.arg(type),
            dots = rlang::dots_list(...),
            class = "add_confidence_interval")
}

#' @export
ggplot_add.add_confidence_interval <- function (object, plot, ...) {
  update_add_confidence_interval(plot, object)
}


update_add_confidence_interval <- function(p, add_confidence_interval_empty_list) {
  # confirm class and structure of object
  .is_ggsurvfit(p, fun_name = "add_confidence_interval()", required_cols = c("conf.low", "conf.high"))

  # getting user-passed arguments
  type <- attr(add_confidence_interval_empty_list, "type")
  dots <- attr(add_confidence_interval_empty_list, "dots")

  # preparing named list of geom arguments
  geom_args <-
    switch(type,
           "ribbon" = list(na.rm = TRUE, alpha = 0.2, color = NA),
           "lines" = list(na.rm = TRUE, linetype = "dashed")
    ) %>%
    utils::modifyList(val = dots)

  # add either the step ribbon or the geom lines for confidence intervals
  p +
    rlang::expr(
      !!switch(
        type,
        "ribbon" =
          rlang::inject(
            stat_stepribbon(
              ggplot2::aes(!!!.construct_ci_aes(p, list(ymin = rlang::expr(.data$conf.low), ymax = rlang::expr(.data$conf.high)), ribbon = TRUE)),
              !!!geom_args
            )
          )
        ,
        "lines" =
          rlang::inject(
            list(
              ggplot2::geom_step(ggplot2::aes(!!!.construct_ci_aes(p, list(y = rlang::expr(.data$conf.low)))), !!!geom_args),
              ggplot2::geom_step(ggplot2::aes(!!!.construct_ci_aes(p, list(y = rlang::expr(.data$conf.high)))), !!!geom_args)
            )
          )
      )
    )
}

# prepare `aes()` call
.construct_ci_aes <- function(p, lst_aes = NULL, ribbon = FALSE) {
  lst_aes <-
    suppressWarnings(ggplot2::ggplot_build(p))$plot$layers[[1]]$computed_mapping[c("x", "color", "colour", "linetype")] %>%
    Filter(Negate(is.null), .) %>% # remove NULLs from list
    utils::modifyList(val = lst_aes)

  # replace `colour` with `fill` if it's a ribbon
  if (ribbon) {
    lst_names <- names(lst_aes)
    lst_names[lst_names %in% c("colour", "color")] <- "fill"
    lst_aes <- stats::setNames(lst_aes, lst_names)
  }

  lst_aes
}
