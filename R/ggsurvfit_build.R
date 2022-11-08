#' Build 'ggsurvfit' Object
#'
#' @description
#' Function takes an object created with `ggsurvfit()` or `ggcuminc()` and
#' prepares the plot for printing.
#' If a plot also has a risk table, this function will build the risk table
#' plots and return them either as list of plots or combined
#' using `patchwork::wrap_plots()`.
#'
#' This can be particularly useful when you would like to place
#' figures with risk tables side-by-side.
#'
#' @param x an object of class 'ggsurvfit' or 'ggcuminc'
#' @param combine_plots logical indicating whether to combine the primary plot
#' and the risk tables. When `TRUE`, plot and risk table(s) are combined with
#' `patchwork::wrap_plots()`.
#' When `FALSE` and the plot has risk tables, they are returned in a list of
#' gtable grobs.
#' Default is `TRUE`.
#'
#' @return a list of ggplot2 objects or a single ggplot2 object
#' @export
#'
#' @examples
#' # construct plot
#' p <-
#'   survfit2(Surv(time, status) ~ surg, df_colon) %>%
#'   ggsurvfit() +
#'   add_risktable() +
#'   scale_y_continuous(limits = c(0, 1))
#'
#' # build plots
#' built_p <- ggsurvfit_build(p, combine_plots = FALSE)
#'
#' # reconstruct original figure print with risktables
#' patchwork::wrap_plots(
#'   built_p[[1]],
#'   built_p[[2]],
#'   built_p[[3]],
#'   ncol = 1,
#'   heights = c(0.70, 0.15, 0.15)
#' )
#'
#' # place plots side-by-side (plots must be built before placement with patchwork)
#' patchwork::wrap_plots(
#'   ggsurvfit_build(p),
#'   ggsurvfit_build(p),
#'   ncol = 2
#' )
#' @inherit ggsurvfit seealso
ggsurvfit_build <- function(x, combine_plots = TRUE) {
  risktable_args <- .extract_arguments_from_attr(x, attr_name = "add_risktable")
  risktable_symbol_args <- .extract_arguments_from_attr(x, attr_name = "add_risktable_strata_symbol")

  if (rlang::is_empty(risktable_args) && !rlang::is_empty(risktable_symbol_args)) {
    cli_inform(c("i" = "{.code add_risktable()} must be run before {.code add_risktable_strata_symbol()}.",
                 "i" = "{.code add_risktable_strata_symbol()} has been ignored."))
    risktable_symbol_args <- NULL
  }

  if (!rlang::is_empty(risktable_args)) {
    # construct and add risktable
    x_eval <- rlang::inject(.construct_risktable(x, !!!risktable_args,
                                                 combine_plots = combine_plots,
                                                 risktable_symbol_args = risktable_symbol_args))
  } else {
    # remove ggsurvfit class, and print with default method
    x_eval <- structure(x, class = setdiff(class(x), c("ggsurvfit", "ggcuminc")))
  }

  # print and return object
  x_eval
}
