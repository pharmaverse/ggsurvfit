#' Survfit Plot Themes
#'
#' @description
#' Returns ggplot list of calls defining a theme.
#'
#' @param theme must be one of `c("none")`
#' - `"none"` no styling applied to risk table
theme_ggsurvfit <- function(theme = c("default", "none")) {
  theme <- match.arg(theme)

  themes_survfit <-
    list(
      "default" =
        list(
          ggplot2::theme_bw()
        ),
      "none" = list()
    )

  themes_survfit[[theme]]
}
