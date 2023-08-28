#' @importFrom rlang %||% .data .env :=
#' @importFrom cli cli_abort cli_warn cli_inform
#' @aliases ggsurvfit-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


utils::globalVariables(".")


release_bullets <- function() {
  c("Remember to build the pkgdown website at `ddsjoberg/ggsurvfit`.")
}
