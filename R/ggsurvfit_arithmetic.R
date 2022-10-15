# YOU MUST HAVE THIS VERSION INSTALLED FOR THIS TO WORK
# remotes::install_github('ddsjoberg/patchwork@arithmetic_prep')

#' Patchwork Arithmetic Prep
#'
#' @inheritParams patchwork::arithmetic_prep
#'
#' @rdname arithmetic_prep
#' @name arithmetic_prep
#' @keywords internal
#'
#' @examples
#' p <-
#'   survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_risktable()
#'
#' p | p | p
NULL

#' @export
#' @rdname arithmetic_prep
arithmetic_prep.ggsurvfit <- function(x, ...) {
  build_and_wrap(x) %>%
    arithmetic_prep()
}

#' @export
#' @rdname arithmetic_prep
arithmetic_prep.ggcuminc <- arithmetic_prep.ggsurvfit

#' @export
#' @rdname arithmetic_prep
arithmetic_prep.ggsurvfit_build <- function(x, ...) {
  build_and_wrap(x) %>%
    arithmetic_prep()
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
