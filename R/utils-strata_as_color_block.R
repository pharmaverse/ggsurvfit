
# function returns a named vector the the strata level as the name and the hex color as the value
.match_strata_level_to_color <- function(plot_build, risktable_group) {
  if (risktable_group == "strata" || !"colour" %in% names(plot_build$data[[1]])) {
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

.construct_color_block <- function() {
  if (!exists("color_block_mapping", envir = rlang::caller_env())) return(NULL)
  color_block_mapping <- get("color_block_mapping", envir = rlang::caller_env())

  list(
    ggplot2::scale_y_discrete(label = function(x) "\U25AC"), # https://cloford.com/resources/charcodes/utf-8_geometric.htm
    ggplot2::theme(
      axis.text.y.left =
        ggplot2::element_text(color = rev(color_block_mapping),
                              size = 15, face = "bold", vjust = 0.4)
    )
  )
}
