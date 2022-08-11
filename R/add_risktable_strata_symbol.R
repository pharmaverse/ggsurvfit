#' Use Symbol for Strata in Risk Table
#'
#' @param symbol UTF-8 code of shape to replace strata level with.
#' Default is a rectangle (`"\U25AC"`).
#' Other common options are circle (`"\U25CF"`) and diamond (`"\U25C6"`)
#' @param size,face,vjust,... arguments passed to `ggtext::element_markdown()`
#' to style the symbol
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' p <-
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit(size = 1) +
#'   add_confidence_interval() +
#'   add_risktable(risktable_group = "risktable_stats")
#'
#'  p + add_risktable_strata_symbol()
#'  p + add_risktable_strata_symbol(symbol = "\U25CF")

add_risktable_strata_symbol <- function(symbol = NULL, size = 15, face = "bold", vjust = 0.45, ...) {
  rlang::inject(
    ggplot2::layer(
      data = NULL, mapping = NULL,
      stat = StatBlankSurvfit, geom = "blank",
      position = "identity",
      show.legend = NA, inherit.aes = TRUE,
      params = list()
    ) %>%
      structure(
        "add_risktable_strata_symbol" =
          list(symbol = symbol %||% "\U25AC",
               size = size, face = face, vjust = vjust,
               !!!rlang::dots_list(...))
      )
  )
}



# function returns a named vector the the strata level as the name and the hex color as the value
.match_strata_level_to_color <- function(plot_build, risktable_group, risktable_symbol_args) {
  if (rlang::is_empty(risktable_symbol_args) ||
      risktable_group == "strata" ||
      !"colour" %in% names(plot_build$data[[1]]) ||
      !"strata" %in% names(plot_build$plot$data)) {
    if (!rlang::is_empty(risktable_symbol_args)) {
      cli_inform(
        c("!" = "Call to {.code add_risktable_strata_symbol()} has been ignored.",
          "i" = "Use this function with {.code add_risktable(risktable_group='risktable_stats')} when stratum are present."))
    }
    return(NULL)
  }

  # find the colors used in the figure
  colors <-
    plot_build$data[[1]] %>%
    dplyr::select(.data$colour) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # try to map group ID to the data strata
  # all strata should be factors and therefore can just extract the
  color_label <-
    plot_build$plot$data %>%
    dplyr::pull(.data$strata) %>%
    levels()

  # i hope this will work 100% of the time!
  # what modifications could a use make the the figures that could break this connection?
  colors %>% stats::setNames(color_label)
}

.construct_color_block <- function(color_block_mapping, symbol, ...) {
  if (rlang::is_empty(color_block_mapping)) return(NULL)
  rlang::check_installed(pkg = "ggtext")


  list(
    ggplot2::scale_y_discrete(
      # label = symbol
      label = Vectorize(function(x) paste0("<span style='color: ", color_block_mapping[[x]],"'>", symbol, "</span>"), "x")
    ), # https://cloford.com/resources/charcodes/utf-8_geometric.htm
    ggplot2::theme(
      axis.text.y.left =
        ggtext::element_markdown(...)
    )
  )
}
