#' Risk Table Themes
#'
#' @description
#' Returns ggplot list of calls defining a theme meant to be applied to a risk table.
#'
#' @param axis.text.y.size text size of the labels on the left of the risk table
#' @param plot.title.size text size of the risk table title
#'
#' @name theme_risktable
#' @return a ggplot2 figure
#' @examples
#' p <- survfit2(Surv(time, status) ~ 1, data = df_lung) %>% ggsurvfit()
#'
#' # default ------------------------------------
#' p + add_risktable(theme = theme_risktable_default())
#'
#' # larger text --------------------------------
#' p +
#'   add_risktable(
#'     size = 4,
#'     theme = theme_risktable_default(axis.text.y.size = 12,
#'                                     plot.title.size = 14)
#'   )
#'
#' # boxed --------------------------------------
#' p + add_risktable(theme = theme_risktable_boxed())
#'
#' # none ---------------------------------------
#' p + add_risktable(theme = NULL, risktable_height = 0.20)
NULL

#' @export
#' @name theme_risktable
theme_risktable_default <- function(axis.text.y.size = 10, plot.title.size = 10.75) {
  list(
    ggplot2::theme_bw(),
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 9, vjust = 1, hjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = axis.text.y.size, colour = "black", face = "plain"),
      plot.margin = ggplot2::unit(c(1, 0, 0, 0), "lines"),
      plot.title = ggplot2::element_text(hjust = 0, vjust = 0, size = plot.title.size),
      legend.position = "none"
    ),
    ggplot2::xlab(NULL),
    ggplot2::ylab(NULL)
  )
}

#' @export
#' @name theme_risktable
theme_risktable_boxed <- function(axis.text.y.size = 10, plot.title.size = 10.75) {
  list(
    theme_risktable_default(axis.text.y.size = axis.text.y.size,
                            plot.title.size = plot.title.size),
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA))
  )
}

