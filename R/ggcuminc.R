#' Plot Cumulative Incidence
#'
#' Plot a cumulative incidence object created with `tidycmprsk::cuminc()`.
#'
#' @param outcome string indicating which outcome(s) to include in plot.
#' Default is to include the first competing event.
#' @inheritParams ggsurvfit
#' @inheritParams tidy_cuminc
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' library(tidycmprsk)
#'
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   ggcuminc(outcome = "death from cancer") +
#'   add_confidence_interval() +
#'   add_risktable()
#'
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   ggcuminc(outcome = c("death from cancer", "death other causes")) +
#'   add_risktable()

ggcuminc <- function(x, outcome = NULL,
                     linetype_aes = FALSE,
                     theme = theme_ggsurvfit_default(), ...) {
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
    if (!identical(Sys.getenv("TESTTHAT"), "true"))
      cli_inform("Plotting outcome {.val {outcome}}.")
  }
  if (any(!outcome %in% unique(df$outcome))) {
    cli_abort("Argument {.code outcome} must be in {.val {unique(df$outcome)}}")
  }
  df <- dplyr::filter(df, .data$outcome %in% .env$outcome)

  # construct aes() call -------------------------------------------------------
  aes_args <- .construct_aes(df, linetype_aes = linetype_aes, outcome = outcome)

  # construction ggplot object -------------------------------------------------
  gg <- .construct_ggplot(x = x, df = df, aes_args = aes_args, theme = theme, ...)

  # assign class and return object ---------------------------------------------
  class(gg) <- c("ggcuminc", class(gg))

  gg
}
