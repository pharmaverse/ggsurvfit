#' Use Symbol for Strata in Risk Table
#'
#' Replace the stratum level names with a color symbol in the risk tables.
#' Use this function when stratum level names are long.
#'
#' @param symbol [UTF-8 code](https://en.wikipedia.org/wiki/UTF-8) of shape to
#' replace strata level with. Default is a rectangle (`"\U25AC"`).
#' Other common options are circle (`"\U25CF"`) and diamond (`"\U25C6"`).
#' While a symbol is the most common string to pass here, any string is acceptable.
#' @param size,face,vjust,... arguments passed to a function similar to
#' `ggtext::element_markdown()`
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examplesIf !names(grDevices::dev.cur()) %in% c("pdf", "postscript")
#' p <-
#'   survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit(linewidth = 1) +
#'   add_confidence_interval() +
#'   add_risktable(risktable_group = "risktable_stats") +
#'   scale_ggsurvfit()
#'
#'  p + add_risktable_strata_symbol()
#'  p + add_risktable_strata_symbol(symbol = "\U25CF", size = 10)
#' @inherit ggsurvfit seealso
add_risktable_strata_symbol <- function(symbol = NULL, size = 15, face = "bold", vjust = 0.3, ...) {
  add_risktable_strata_symbol_empty_list <- list()
  rlang::inject(
    structure(add_risktable_strata_symbol_empty_list,
              "add_risktable_strata_symbol" =
                list(symbol = symbol %||% "\U25AC",
                     face = face,
                     vjust = vjust,
                     size = size,
                     !!!rlang::dots_list(...)
                ),
              class = "add_risktable_strata_symbol")
  )
}

#' @export
ggplot_add.add_risktable_strata_symbol <- function (object, plot, ...) {
  update_add_risktable_strata_symbol(plot, object)
}

update_add_risktable_strata_symbol <- function(p, add_risktable_strata_symbol_empty_list) {
  # confirm class and structure of object
  .is_ggsurvfit(p, fun_name = "add_risktable_strata_symbol()")

  p +
    rlang::inject(
      structure(
        ggplot2::geom_blank(),
        add_risktable_strata_symbol =
          !!attr(add_risktable_strata_symbol_empty_list, "add_risktable_strata_symbol")
      )
    )
}


# function returns a named vector the the strata level as the name and the hex color as the value
.match_strata_level_to_color <- function(plot_build, risktable_group, risktable_symbol_args) {
  if (rlang::is_empty(risktable_symbol_args) ||
      risktable_group == "strata"  ||
      !"colour" %in% names(plot_build$data[[1]])) {
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
    dplyr::select("colour") %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # try to map group ID to the data strata
  # all strata should be factors and therefore can just extract the
  color_label <-
    switch(
      "strata" %in% names(plot_build$plot$data),
      plot_build$plot$data %>%
        dplyr::pull("strata") %>%
        levels()
    ) %||%
    "Overall"

  rep_len(colors, length.out = length(color_label)) %>% stats::setNames(color_label)
}

.construct_color_block <- function(color_block_mapping, symbol, ...) {
  if (rlang::is_empty(color_block_mapping)) return(NULL)

  list(
    ggplot2::scale_y_discrete(
      label = function(x) symbol # https://cloford.com/resources/charcodes/utf-8_geometric.htm
    ),
    ggplot2::theme(
      axis.text.y.left =
        ggplot2::element_text(color = rev(color_block_mapping), ...)
    )
  )
}
