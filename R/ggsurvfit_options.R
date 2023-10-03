#' Global Options
#'
#' @description
#' By default, `ggsurvfit()` and `ggcuminc()` uses the color aesthetic to draw
#' curves stratified by treatment group. Moreover, in `ggcuminc()` when multiple
#' outcomes are plotted on the same figure the linetype aesthetic is used to
#' distinguish the curves among the various outcomes.
#'
#' It is, however, sometimes desirable to use the linetype to stratify
#' by treatment group and color by outcome. To obtain these figures, set the
#' `options("ggsurvfit.switch-color-linetype" = TRUE)` option.
#'
#' @name ggsurvfit_options
#' @examples
#' options("ggsurvfit.switch-color-linetype" = TRUE)
#' library(tidycmprsk)
#'
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   ggcuminc(outcome = "death from cancer") +
#'   add_risktable()
#'
#' cuminc(Surv(ttdeath, death_cr) ~ 1, trial) %>%
#'   ggcuminc(outcome = c("death from cancer", "death other causes")) +
#'   add_risktable()
#'
#' # reset option
#' options("ggsurvfit.switch-color-linetype" = NULL)
NULL
