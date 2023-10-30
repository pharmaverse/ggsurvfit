#' Align Plots
#'
#' Function accepts a list of ggplot objects, and aligns each plot to the same
#' widths as the first passed plot. This utility function is used to align
#' the risktable plots with the risk curve plots.
#'
#' @param pltlist list of ggplots
#'
#' @return a list of ggplot grobs
#' @export
#'
#' @examples
#' # construct a base plot
#' gg <-
#'   survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_confidence_interval() +
#'   scale_ggsurvfit()
#'
#' # create an area plot representing the number of subjects who experienced
#' df_risktable <-
#'   survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   tidy_survfit()
#' # the event and those that have been censored.
#' gg_risktable_figure <-
#'   df_risktable %>%
#'   ggplot() +
#'   geom_ribbon(aes(x = time, ymin = 0, ymax = cum.event), fill = "black") +
#'   geom_ribbon(aes(x = time, ymin = n.risk[1], ymax = n.risk[1] - cum.censor), fill = "grey") +
#'   theme_void() +
#'   theme(axis.text.y = element_text(size=8)) +
#'   scale_y_continuous(
#'     breaks = c(0, max(df_risktable$n.risk)),
#'     labels = c("Cum. Events",  "Cum.Censored")
#'   )
#'
#' # align plots
#' lst_aligned_plots <- ggsurvfit_align_plots(list(gg, gg_risktable_figure))
#'
#' # combine plots with patchwork
#' patchwork::wrap_plots(
#'   lst_aligned_plots,
#'   ncol = 1,
#'   heights = c(0.9, 0.1)
#' )
ggsurvfit_align_plots <- function(pltlist) {
  # set all x axis ranges to be the same
  x_range <-
    ggplot2::ggplot_build(pltlist[[1]])$layout$panel_params[[1]]$x.range
  for (i in setdiff(seq_along(pltlist), 1L)) {
    y_range <-
      ggplot2::ggplot_build(pltlist[[i]])$layout$panel_params[[1]]$y.range

    pltlist[[i]] <-
      pltlist[[i]] +
      ggplot2::coord_cartesian(
        xlim = x_range,
        ylim = y_range,
        expand = FALSE
      )
  }

  # turn plots into grobs and determine number of columns
  plots_grobs <- lapply(pltlist, ggplot2::ggplotGrob)
  ncols <- lapply(plots_grobs, function(x) dim(x)[[2]])
  maxcols <- max(unlist(ncols))

  # Function to add more columns to compensate for eg missing legend
  .addcols <- function(x) {
    diffcols <- maxcols - dim(x)[[2]]

    if (diffcols > 0) {
      for (i in seq(1:diffcols)) {
        x <- gtable::gtable_add_cols(x, widths = grid::unit(1, "null"), pos = 8)
      }
    }

    x
  }

  ### TableGrob 1 has 11 columns while the others have only 9 because lacking legend+spacer
  ## => add two columns and then resize
  plots_grobs_xcols <- lapply(plots_grobs, .addcols)

  ### assign max length to ensure alignment
  max_width <- do.call(grid::unit.pmax, lapply(plots_grobs_xcols, "[[", "widths"))
  for (i in seq(1, length(plots_grobs_xcols))) {
    plots_grobs_xcols[[i]]$widths <- max_width
  }

  xcol_widths <- grid::convertWidth(
    plots_grobs_xcols[[1]]$widths,
    unitTo = "cm",
    valueOnly = FALSE
  )
  grob_widths <- grid::convertWidth(
    plots_grobs[[1]]$widths,
    unitTo = "cm",
    valueOnly = FALSE
  )
  x <- xcol_widths[[4]] - grob_widths[[4]]

  plots_grobs_xcols[[1]]$grobs[[13]]$children[[1]]$x <- grid::unit(x, "cm")

  plots_grobs_xcols
}
