#' Tidy a cuminc object
#'
#' The tidycmprsk package exports a tidier for `"cuminc"` objects.
#' This function adds on top of that and returns more information.
#'
#' @param x a 'cuminc' object created with `tidycmprsk::cuminc()`
#' @param times numeric vector of times. Default is `NULL`,
#' which returns all observed times.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(tidycmprsk)
#'
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   tidy_cuminc()
tidy_cuminc <- function(x, times = NULL) {
  rlang::check_installed("tidycmprsk", version = "0.2.0")

  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "tidycuminc")) {
    cli_abort(c("!" = "Argument {.code x} must be class {.cls tidycuminc}.",
                "i" = "Create the object with {.code tidycmprsk::cuminc()}"))
  }

  # create base tidy tibble ----------------------------------------------------
  df_tidy <- tidycmprsk::tidy(x, times = times)

  # improve strata label, if possible ------------------------------------------
  df_tidy <- .construct_strata_label(df_tidy, survfit = x)

  # return tidied tibble
  df_tidy %>%
    dplyr::mutate(
      estimate_type = "cuminc",
      estimate_type_label = "Cumulative Incidence",
      monotonicity_type = "increasing"
    )
}
