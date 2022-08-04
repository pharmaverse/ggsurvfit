#' Risk Table Themes
#'
#' @description
#' Returns ggplot list of calls defining a theme.
#'
#' @param theme must be one of `c("default", "boxed", "none")`
#' - `"default"` a clean, simple default theme
#' - `"boxed"` the default theme with box around risk table
#' - `"none"` no styling applied to risk table

theme_ggsurvfit_risktable <- function(theme = c("default", "boxed", "none")) {
  theme <- match.arg(theme)

  theme_risktable_default <-
    list(
      ggplot2::theme_bw(),
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = 8,
          vjust = 1,
          hjust = 1
        ),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 8, colour = "black", face = "plain"),
        plot.margin = ggplot2::unit(c(1, 0, 0, 0), "lines"),
        plot.title = ggplot2::element_text(hjust = 0, vjust = 0, size = 10),
        legend.position = "none"
      ),
      ggplot2::xlab(NULL),
      ggplot2::ylab(NULL)
    )

  themes_risktable <-
    list(
      "default" = theme_risktable_default,
      "boxed" =
        c(
          theme_risktable_default,
          list(ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA)))
        ),
      "none" = list()
    )

  themes_risktable[[theme]]
}
