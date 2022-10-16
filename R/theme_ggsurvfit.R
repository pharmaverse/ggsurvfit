#' Survfit Plot Themes
#'
#' @description
#' Returns ggplot list of calls defining a theme.
#' - `theme_ggsurvfit_default()`: Builds on `theme_bw()` with increased text sizes.
#' - `theme_ggsurvfit_KMunicate()`: Theme to create KMunicate-styled figures. \doi{10.1136/bmjopen-2019-030215}
#'
#' @return a ggplot2 theme
#' @name theme_ggsurvfit
#' @examples
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit(theme = theme_ggsurvfit_default())
NULL

#' @export
#' @rdname theme_ggsurvfit
theme_ggsurvfit_default <- function() {
  list(
    ggplot2::theme_bw(),
    ggplot2::theme(legend.position = "bottom"),
    ggplot2::theme(strip.background = ggplot2::element_blank()),
    ggplot2::theme(axis.text = ggplot2::element_text(size = 10)),
    ggplot2::theme(axis.title = ggplot2::element_text(size = 12)),
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
  )
}

#' @export
#' @rdname theme_ggsurvfit
theme_ggsurvfit_KMunicate <- function() {
  list(
    theme_ggsurvfit_default(),
    ggplot2::theme_minimal(),
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ),
    ggplot2::labs(color = NULL, fill = NULL, linetype = NULL)
  )
}
