#' Plot Cumulative Incidence
#'
#' @description
#' Plot a cumulative incidence object created with `tidycmprsk::cuminc()`
#' or a multi-state object created with `survfit2()`.
#' Read more on multi-state models [here](https://cran.r-project.org/package=survival/vignettes/compete.pdf).
#'
#' @param outcome string indicating which outcome(s) to include in plot.
#' Default is to include the first competing event.
#' @inheritParams ggsurvfit
#' @inheritParams tidy_cuminc
#'
#' @return a ggplot2 figure
#' @export
#'
#' @section Details:
#'
#' *Why not use `cmprsk::cuminc()`?*
#'
#' The implementation of `cmprsk::cuminc()` does not provide the data required
#' to construct the risk table. Moreover, the `tidycmprsk::cuminc()` has a
#' user-friendly interface making it easy to learn and use.
#'
#' @examples
#' \donttest{
#' library(tidycmprsk)
#'
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   ggcuminc(outcome = "death from cancer") +
#'   add_confidence_interval() +
#'   add_risktable() +
#'   scale_ggsurvfit()
#'
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   ggcuminc(outcome = c("death from cancer", "death other causes")) +
#'   add_risktable() +
#'   scale_ggsurvfit()
#'
#' # using the survival multi-state model
#' survfit2(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   ggcuminc(outcome = "death from cancer") +
#'   add_confidence_interval() +
#'   add_risktable() +
#'   scale_ggsurvfit()
#' }
#' @inherit ggsurvfit seealso
ggcuminc <- function(x, outcome = NULL,
                     linetype_aes = FALSE,
                     theme = theme_ggsurvfit_default(), ...) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, c("tidycuminc", "survfitms"))) {
    cli_abort(
      c(
        "!" = "Argument {.code x} must be {.cls tidycuminc}.",
        "i" = "Create the object with {.code tidycmprsk::cuminc()}."
      )
    )
  }

  # prep data to be passed to ggplot() -----------------------------------------
  if (inherits(x, "tidycuminc")) {
    df <- tidy_cuminc(x = x)
  } else if (inherits(x, "survfitms")) {
    df <- tidy_survfit(x = x)
  }

  # subset on outcome of interest ----------------------------------------------
  if (is.null(outcome)) {
    outcome <- df$outcome[1]
    cli_inform("Plotting outcome {.val {outcome}}.")
  }
  if (any(!outcome %in% unique(df$outcome))) {
    cli_abort("Argument {.code outcome} must be one or more of {.val {unique(df$outcome)}}")
  }
  df <- dplyr::filter(df, .data$outcome %in% .env$outcome)

  # adding the model object to df
  df <- df %>% dplyr::mutate(survfit = c(list(x), rep_len(list(), dplyr::n() - 1L)))

  # construct aes() call -------------------------------------------------------
  aes_args <- .construct_aes(df, linetype_aes = linetype_aes, outcome = outcome)

  # construction ggplot object -------------------------------------------------
  gg <- .construct_ggplot(x = x, df = df, aes_args = aes_args, theme = theme, ...)

  # assign class and return object ---------------------------------------------
  class(gg) <- c("ggcuminc", class(gg))

  gg
}
