#' Plot Arithmetic
#'
#' @description
#' Plot arithmetic for 'ggsurvfit' figures, built to mimic the
#' [patchwork plot arithmetic](https://patchwork.data-imaginist.com/reference/plot_arithmetic.html).
#' Methods for `|`, `-`, and `/` have been implemented.
#'
#' Use these functions to patch together figures created with the ggsurvfit package.
#' If you need patch together 'ggsurvfit' plots with other plots, see details below.
#'
#' Notably, you **cannot** use the `+` operator to patch 'ggsurvfit' plots together:
#' use `|` instead.
#'
#' @section Details:
#'
#' A few details on ggsurvfit plot arithmetic:
#' 1. These functions are meant to only work with figures created with the ggsurvfit package.
#' 2. Not every combination of operators is supported by the ggsurvift
#'    implementation of the plot operators. To utilize the full suite supported
#'    by patchwork, see the note below.
#' 3. If you need to patch together 'ggsurvfit' figures with non-ggsurvfit
#'    figures OR you need a feature supported in patchwork that is not supported
#'    by the ggsurvfit implementation, you must utilize the arithmetic
#'    operators directly from the `patchwork` package.
#'    To use directly use the `patchwork` operators with a figure created
#'    with `ggsurvfit`, it must be wrapped in `patchwork::wrap_elements(ggsurvfit_build(p))`.
#'
#'    ```r
#'    patchwork::wrap_elements(ggsurvfit_build(p)) + patchwork::wrap_elements(ggsurvfit_build(p))
#'    ```
#'
#' @param e1 A `ggsurvfit`, `ggcuminc`, or `ggsurvfit_built` object
#' @param e2 A `ggsurvfit`, `ggcuminc`,`ggsurvfit_built`, `ggplot` or `patchwork`
#' object in case of `/`, or a `gg` object such as a geom or
#' theme specification in case of `*` and `&`
#'
#' @name ggsurvfit_arithmetic
#' @rdname ggsurvfit_arithmetic
#'
#' @return A `patchwork` object
#' @examples
#' p <-
#'   survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_risktable()
#'
#' p1 <- p + ggtitle("Plot 1")
#' p2 <- p + ggtitle("Plot 2")
#'
#' p1 | p2
#'
#' p1 - p2
NULL

# `|` methods ------------------------------------------------------------------
#' @rdname ggsurvfit_arithmetic
#' @export
"|.ggsurvfit_build" <- function(e1, e2) {
  build_and_wrap(e1) | build_and_wrap(e2)
}

#' @rdname ggsurvfit_arithmetic
#' @export
"|.ggsurvfit" <- function(e1, e2) {
  build_and_wrap(e1) | build_and_wrap(e2)
}

#' @rdname ggsurvfit_arithmetic
#' @export
"|.ggcuminc" <- function(e1, e2) {
  build_and_wrap(e1) | build_and_wrap(e2)
}

# `-` methods ------------------------------------------------------------------
#' @rdname ggsurvfit_arithmetic
#' @export
"-.ggsurvfit_build" <- function(e1, e2) {
  build_and_wrap(e1) - build_and_wrap(e2)
}

#' @rdname ggsurvfit_arithmetic
#' @export
"-.ggsurvfit" <- function(e1, e2) {
  build_and_wrap(e1) - build_and_wrap(e2)
}

#' @rdname ggsurvfit_arithmetic
#' @export
"-.ggcuminc" <- function(e1, e2) {
  build_and_wrap(e1) - build_and_wrap(e2)
}

# `/` methods ------------------------------------------------------------------
#' @rdname ggsurvfit_arithmetic
#' @export
"/.ggsurvfit_build" <- function(e1, e2) {
  build_and_wrap(e1) / build_and_wrap(e2)
}

#' @rdname ggsurvfit_arithmetic
#' @export
"/.ggsurvfit" <- function(e1, e2) {
  build_and_wrap(e1) / build_and_wrap(e2)
}

#' @rdname ggsurvfit_arithmetic
#' @export
"/.ggcuminc" <- function(e1, e2) {
  build_and_wrap(e1) / build_and_wrap(e2)
}

build_and_wrap <- function(x) {
  if (inherits(x, c("ggsurvfit", "ggcuminc"))) {
    x <- ggsurvfit_build(x)
  }
  if (inherits(x, "ggsurvfit_build")) {
    x <- patchwork::wrap_elements(x)
  }
  x
}

