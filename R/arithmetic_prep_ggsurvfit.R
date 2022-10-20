# YOU MUST HAVE THIS VERSION INSTALLED FOR THIS TO WORK
# remotes::install_github('ddsjoberg/patchwork@arithmetic_prep')

#' Patchwork Arithmetic Prep
#'
#' @inheritParams patchwork::arithmetic_prep
#'
#' @rdname arithmetic_prep_ggsurvfit
#' @name arithmetic_prep_ggsurvfit
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
#' @rdname arithmetic_prep_ggsurvfit
arithmetic_prep.ggsurvfit <- function(x, ...) {
  build_and_wrap(x) %>%
    arithmetic_prep()
}

#' @export
#' @rdname arithmetic_prep_ggsurvfit
arithmetic_prep.ggcuminc <- arithmetic_prep.ggsurvfit

#' @export
#' @rdname arithmetic_prep_ggsurvfit
arithmetic_prep.ggsurvfit_build <- arithmetic_prep.ggsurvfit

#' @export
#' @rdname arithmetic_prep_ggsurvfit
ggplot_add.ggsurvfit <- function(object, plot, object_name) {
  build_and_wrap(object, wrap_elements = FALSE) %>%
    ggplot_add(plot = plot, object_name = object_name)
}

#' @export
#' @rdname arithmetic_prep_ggsurvfit
ggplot_add.ggcuminc <- ggplot_add.ggsurvfit

#' @export
#' @rdname arithmetic_prep_ggsurvfit
ggplot_add.ggsurvfit_build <- function(object, plot, object_name) {
  plot + patchwork::wrap_elements(full = object)
}


build_and_wrap <- function(x, wrap_elements = TRUE) {
  if (inherits(x, c("ggsurvfit", "ggcuminc"))) {
    x <- ggsurvfit_build(x)
  }
  if (isTRUE(wrap_elements) && inherits(x, "ggsurvfit_build")) {
    x <- patchwork::wrap_elements(x)
  }
  x
}
