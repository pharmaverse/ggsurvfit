#' Plot Cumulative Incidence
#'
#' @param outcome string indicating which outcome to include in plot.
#' Default is to include the first competing event.
#' @param theme a theme . Default is `theme_ggsurvfit_default()`
#' @param ... arguments passed to `ggplot2::geom_step(...)`, e.g. `size = 2`
#' @inheritParams tidy_cuminc
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' library(tidycmprsk)
#'
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   ggcuminc(outcome = "death from cancer") +
#'   add_confidence_interval() +
#'   add_risktable()
ggcuminc <- function(x, outcome = NULL, theme = theme_ggsurvfit_default(), ...) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "tidycuminc")) {
    cli_abort(
      c("!" = "Argument {.code x} must be {.cls tidycuminc}.",
        "i" = "Create the object with {.code tidycmprsk::cuminc()}.")
    )
  }

  # prep data to be passed to ggplot() -----------------------------------------
  df <- tidy_cuminc(x = x)

  # subset on outcome of interest ----------------------------------------------
  if (is.null(outcome)) {
    outcome <- df$outcome[1]
    cli_inform("Plotting outcome {.val {outcome}}.")
  }
  if (!rlang::is_string(outcome) || !outcome %in% unique(df$outcome)) {
    cli_abort("Argument {.code outcome} must be one of {.val {unique(df$outcome)}}")
  }
  df <- dplyr::filter(df, .data$outcome %in% .env$outcome)

  # construct aes() call -------------------------------------------------------
  aes_args <- .construct_aes(df)

  # construction ggplot object -------------------------------------------------
  gg <- .construct_ggplot(x = x, df = df, aes_args = aes_args, theme = theme, ...)

  # assign class and return object ---------------------------------------------
  class(gg) <- c("ggcuminc", class(gg))

  gg
}
