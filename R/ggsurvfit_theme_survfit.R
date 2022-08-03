#' Survfit Plot Themes
#'
#' @description
#' Returns ggplot list of calls defining a theme.
#'
#' @param theme must be one of `c("none")`
#' - `"none"` no styling applied to risk table
theme_ggsurvfit_survfit <- function(theme = c("none")) {
  theme <- match.arg(theme)


  themes_survfit <-
    list(
      "none" = list()
    )

  themes_survfit[[theme]]
}
