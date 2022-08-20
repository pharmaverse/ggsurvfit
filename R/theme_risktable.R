#' Risk Table Themes
#'
#' @description
#' Returns ggplot list of calls defining a theme meant to be applied to a risk table.
#'
#' @name theme_risktable
#' @return a ggplot2 figure
#' @examples
#' p <- survfit2(Surv(time, status) ~ 1, data = df_lung) %>% ggsurvfit()
#'
#' # default ------------------------------------
#' p + add_risktable(theme = theme_risktable_default())
#'
#' # boxed --------------------------------------
#' p + add_risktable(theme = theme_risktable_boxed())
#'
#' # none ---------------------------------------
#' p + add_risktable(theme = NULL)
NULL

#' @export
#' @name theme_risktable
theme_risktable_default <- function() {
  list(
    ggplot2::theme_bw(),
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        size = 9,
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
}

#' @export
#' @name theme_risktable
theme_risktable_boxed <- function() {
  list(
    theme_risktable_default(),
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA))
  )
}

