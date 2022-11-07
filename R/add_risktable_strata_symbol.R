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
#' @examples
#' p <-
#'   survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit(size = 1) +
#'   add_confidence_interval() +
#'   add_risktable(risktable_group = "risktable_stats")
#'
#'  p + add_risktable_strata_symbol()
#'  p + add_risktable_strata_symbol(symbol = "\U25CF", size = 10)
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
ggplot_add.add_risktable_strata_symbol <- function (object, plot, object_name) {
  update_add_risktable_strata_symbol(plot, object)
}

update_add_risktable_strata_symbol <- function(p, add_risktable_strata_symbol_empty_list) {
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



# add_risktable_strata_symbol <- function(symbol = NULL, size = 15, face = "bold", vjust = 0.3, ...) {
#   rlang::inject(
#     ggplot2::layer(
#       data = NULL, mapping = NULL,
#       stat = StatBlankSurvfit, geom = "blank",
#       position = "identity",
#       show.legend = NA, inherit.aes = TRUE,
#       params = list()
#     ) %>%
#       structure(
#         "add_risktable_strata_symbol" =
#           list(symbol = symbol %||% "\U25AC",
#                size = size, face = face, vjust = vjust,
#                !!!rlang::dots_list(...))
#       )
#   )
# }



# function returns a named vector the the strata level as the name and the hex color as the value
.match_strata_level_to_color <- function(plot_build, risktable_group, risktable_symbol_args) {
  if (rlang::is_empty(risktable_symbol_args) ||
      risktable_group == "strata"  ||
      # !"strata" %in% names(plot_build$plot$data) ||
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

  # i hope this will work 100% of the time!
  # what modifications could a use make the the figures that could break this connection?
  colors %>% stats::setNames(color_label)
}

.construct_color_block <- function(color_block_mapping, symbol, ...) {
  if (rlang::is_empty(color_block_mapping)) return(NULL)

  list(
    ggplot2::scale_y_discrete(
      label = function(x) symbol # https://cloford.com/resources/charcodes/utf-8_geometric.htm
    ),
    ggplot2::theme(
      axis.text.y.left =
        element_text2(color = rev(color_block_mapping), ...)
    )
  )
}

# taken from SO https://stackoverflow.com/questions/73293798
element_text2 <- function(..., color = NULL) {
  # Explicitly don't pass colour
  # Note: user can still pass `colour`, but I'm not here to write perfect code,
  # just to give a working example
  elem <- ggplot2::element_text(...)
  elem$colour <- color # Assign after element is constructed
  class(elem) <- c("element_text2", "element_text", "element") # Re-class
  elem
}

# these lines were not getting hit on code covereage so i comment them out and it still works :)
# # S3 Method for your custom class' drawing code
# element_grob.element_text2 <- function(element, label = "", ...,
#                                        colour = NULL) {
#   # Repeat colour to match length of label, if colour exists
#   if (length(colour)) {
#     colour <- rep_len(colour, length(label))
#   }
#   # Re-class to old class
#   class(element) <- c("element_text", "element")
#   # Call element_grob.element_text method
#   ggplot2::element_grob(element, label = label, ..., colour = colour)
# }
