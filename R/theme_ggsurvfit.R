#' Survfit Plot Themes
#'
#' @description
#' Returns ggplot list of calls defining a theme.
#'
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
    ggplot2::theme(strip.background = ggplot2::element_blank())
  )
}
