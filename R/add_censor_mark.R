#' Add Censor Marking
#'
#' Add a marking on the figure to represent the time an observations was
#' censored.
#'
#' @param ... arguments passed to passed to
#' `ggplot2::geom_point(...)` with defaults `shape = 3` and `size = 2`
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_confidence_interval() +
#'   add_censor_mark() +
#'   scale_ggsurvfit()
#' @inherit ggsurvfit seealso
add_censor_mark <- function(...) {
  add_censor_mark_empty_list <- list()
  structure(add_censor_mark_empty_list,
            dots = utils::modifyList(x = list(na.rm = TRUE, size = 2, shape = 3),
                                     val = rlang::dots_list(...)),
            class = "add_censor_mark")
}

#' @export
ggplot_add.add_censor_mark <- function (object, plot, object_name) {
  update_add_censor_mark(plot, object)
}


update_add_censor_mark <- function(p, add_censor_mark_empty_list) {
  # confirm class and structure of object
  .is_ggsurvfit(p, fun_name = "add_censor_mark()", required_cols = c("time", "estimate", "n.censor"))

  # getting user-passed arguments
  dots <- attr(add_censor_mark_empty_list, "dots")

  # add censor marks with `geom_point()`
  p +
    rlang::inject(
      ggplot2::geom_point(
        data = ~ tidyr::uncount(.x, weights = .data$n.censor),
        ggplot2::aes(!!!.construct_censor_mark_aes(p)),
        !!!dots
      )
    )
}

# prepare `aes()` call
.construct_censor_mark_aes <- function(p) {
  lst_aes <-
    list(
      x = rlang::expr(.data$time),
      y = rlang::expr(.data$estimate)
    )

  # if a stratified model, add a `colour=` argument
  if ("strata" %in% names(ggplot2::ggplot_build(p)$plot$data) &&
      isFALSE(getOption("ggsurvfit.switch-color-linetype", default = FALSE))) {
    lst_aes <- c(lst_aes, list(color = rlang::expr(.data$strata)))
  }
  if ("outcome" %in% names(ggplot2::ggplot_build(p)$plot$data) &&
      isTRUE(getOption("ggsurvfit.switch-color-linetype", default = FALSE))) {
    lst_aes <- c(lst_aes, list(color = rlang::expr(.data$outcome)))
  }

  lst_aes
}
