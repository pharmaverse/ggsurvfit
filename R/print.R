#' Print ggsurvfit object
#'
#' @param x an object of class 'ggsurvfit'
#' @inheritParams rlang::args_dots_empty
#'
#' @return a printed ggplot
#' @name print_ggsurvfit
#' @keywords internal
#'
#' @examples
#' # add example
NULL

#' @export
#' @rdname print_ggsurvfit
print.ggsurvfit <- function(x, ...) {
  risktable_args <- .extract_risktable_arguments(x)

  if (!is.null(risktable_args)) {
    # construct and add risktable
    x_eval <- rlang::inject(.construct_risktable(x, !!!risktable_args))
  } else {
    # remove ggsurvfit class, and print with default method
    x_eval <- structure(x, class = setdiff(class(x), c("ggsurvfit", "ggcuminc")))
  }

  # print and return object
  print(x_eval)
  return(invisible(x_eval))
}

#' @export
#' @rdname print_ggsurvfit
knit_print.ggsurvfit <- function(x, ...) {
  print(x, ...)
}

#' @export
#' @rdname print_ggsurvfit
print.ggcuminc <- function(x, ...) {
  # print and return object
  print.ggsurvfit(x)
}

#' @export
#' @rdname print_ggsurvfit
knit_print.ggcuminc <- function(x, ...) {
  print(x, ...)
}
