#' Deprecated functions
#'
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

#' @export
#' @rdname deprecated
survfit_pvalue <- function(...) {
  cli_abort(c(
    "!" = "The {.code survfit_pvalue()} function was a part of the pre-release and is now {.emph} deprecated.",
    "i" = "Use {.code survfit2_p()} instead."
  ))
}
