#' Survfit Plot Themes
#'
#' @description
#' Returns ggplot list of calls defining a theme.
#'
#' @param theme must be one of `c("default", "none")`
#' - `"default"` the default plot styling
#' - `"none"` no styling applied to risk table; equivalent to using `theme = NULL`
theme_ggsurvfit <- function(theme = c("default", "none")) {
  theme <- match.arg(theme)

  themes_survfit <-
    list(
      "default" =
        list(
          ggplot2::theme_bw(),
          ggplot2::theme(strip.background = ggplot2::element_blank())
        ),
      "none" = list()
    )

  themes_survfit[[theme]]
}
