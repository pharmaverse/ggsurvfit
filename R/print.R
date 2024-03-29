#' Print ggsurvfit object
#'
#' @param x an object of class 'ggsurvfit' or 'ggcuminc'
#' @inheritParams rlang::args_dots_empty
#'
#' @return a printed ggplot2 figure
#' @name print_ggsurvfit
#' @keywords internal
#'
#' @examples
#' print(survfit2(Surv(time, status) ~ surg, data = df_colon))
NULL

#' @export
#' @rdname print_ggsurvfit
print.ggsurvfit <- function(x, ...) {
  # Build plots ----------------------------------------------------------------
  built_x <- ggsurvfit_build(x, combine_plots = TRUE)

  # print and return object ----------------------------------------------------
  print(built_x)
}

#' @export
#' @rdname print_ggsurvfit
print.ggcuminc <- print.ggsurvfit
