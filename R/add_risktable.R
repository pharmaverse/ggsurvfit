#' Add risk tables
#'
#' @param times numeric vector of times where risk table values will be placed.
#' Default are the times shown on the x-axis.
#' @param risktable_stats character vector of statistics to show in the risk table.
#' Must be one or more of `c("n.risk", "cum.event", "cum.censor", "n.event", "n.censor")`.
#' Default is `c("n.risk", "cum.event")`.
#' - `"n.risk"` Number of patients at risk
#' - `"cum.event"` Cumulative number of observed events
#' - `"cum.censor"` Cumulative number of censored observations
#' - `"n.event"` Number of events in time interval
#' - `"n.censor"` Number of censored observations in time interval
#' @param stats_label named vector or list of custom labels. Names are the statistics
#' from `risktable_stats=` and the value is the custom label.
#' @param risktable_group String indicating the grouping variable for the risk tables.
#' Default is `"auto"` and will select `"strata"` or `"risktable_stats"` based on context.
#' - `"strata"` groups the risk tables per stratum when present.
#' - `"risktable_stats"` groups the risk tables per risktable_stats.
#' @param combine_groups logical indicating whether to combine the statistics
#' in the risk table across groups. Default is `FALSE`
#' @param risktable_height A numeric value between 0 and 1 indicates the height used by the table versus the height
#'  used by the plot, as described in `patchwork::wrap_plots(heights=)`. The default is 0.14.
#' @param theme A risktable theme typically returned from `theme_ggsurvfit_risktable()`
#' @param strata_as_color_block logical indicating whether to replace strata levels with color
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' p <-
#'   df_lung %>%
#'   survfit2(Surv(time, status) ~ sex, data = .) %>%
#'   ggsurvfit() +
#'   add_censor_mark() +
#'   add_confidence_interval()
#'
#' p + add_risktable()
#'
#' p +
#'   add_risktable(
#'     risktable_stats = c("n.risk", "cum.event"),
#'     stats_label = list(
#'       cum.event = "Cumulative Observed Events",
#'       n.risk = "Number at Risk"
#'     ),
#'     risktable_group = "strata",
#'   )
#'
#' p +
#'   add_risktable(
#'     risktable_stats = c("n.risk", "cum.event"),
#'     combine_groups = TRUE
#'   )
add_risktable <- function(times = NULL,
                          risktable_stats = c("n.risk", "cum.event"),
                          risktable_group = c("auto", "strata", "risktable_stats"),
                          risktable_height = 0.14,
                          stats_label = NULL,
                          strata_as_color_block = TRUE,
                          combine_groups = FALSE,
                          theme = theme_ggsurvfit_risktable()) {
  rlang::inject(
    ggplot2::layer(
      data = NULL, mapping = NULL,
      stat = StatBlankSurvfit, geom = "blank",
      position = "identity",
      show.legend = NA, inherit.aes = TRUE,
      params = list()
    ) %>%
      structure(
        "risktable_args" = list(
          times = times,
          risktable_stats =
            !!match.arg(
              risktable_stats,
              choices = c("n.risk", "cum.censor", "cum.event", "n.censor", "n.event"),
              several.ok = TRUE
            ),
          stats_label = stats_label,
          strata_as_color_block = strata_as_color_block,
          combine_groups = combine_groups,
          risktable_group = !!match.arg(risktable_group),
          risktable_height = risktable_height,
          theme = theme
        )
      )
  )
}


StatBlankSurvfit <-
  ggplot2::ggproto(
    "StatBlankSurvfit", ggplot2::Stat,
    compute_group = function(data, scales, ...) {
      .is_ggsurvfit(data, fun_name = "add_risktable()", required_aes_cols = c("x", "y"))
      data
    }
  )

.construct_risktable <- function(x, times, risktable_stats, stats_label, group,
                                 combine_groups, risktable_group, strata_as_color_block,
                                 risktable_height, theme, combine_plots) {
  plot_build <- ggplot2::ggplot_build(x)
  times <- times %||% plot_build$layout$panel_params[[1]]$x.sec$breaks

  df_times <-
    .prepare_data_for_risk_tables(data = x$data, times = times, combine_groups = combine_groups)

  if (risktable_group == "auto") {
    risktable_group <-
      dplyr::case_when(
        "strata" %in% names(df_times) & length(risktable_stats) == 1L ~ "risktable_stats",
        TRUE ~ "strata"
      )
  }

  df_stat_labels <- .construct_stat_labels(risktable_stats, stats_label)

  # create list of ggplots, one plot for each risktable
  gg_risktable_list <-
    .create_list_of_gg_risk_tables(
      df_times, risktable_stats, times,
      df_stat_labels, theme, risktable_group,
      color_block_mapping = .match_strata_level_to_color(plot_build, risktable_group)
    )

  # align all the plots
  gg_risktable_list_aligned <-
    c(list(x), gg_risktable_list) %>%
    align_plots()

  if (isFALSE(combine_plots)) return(gg_risktable_list_aligned)

  ## combine all plots into single figure
  gg_final <-
    gg_risktable_list_aligned %>%
    patchwork::wrap_plots(
      ncol = 1,
      heights = c(1 - (risktable_height * (length(gg_risktable_list_aligned) - 1)), rep(risktable_height, length(gg_risktable_list_aligned) - 1))
    )

  gg_final
}

.construct_stat_labels <- function(risktable_stats, stats_label, combine_plots = TRUE) {
  if (!is.null(stats_label) &&
      !rlang::is_named(stats_label) &&
      length(risktable_stats) != length(stats_label)) {
    cli_abort(
      c("When {.var stats_label} is not a named list, it must be the same length as {.var risktable_stats}.",
        "i" = "{.var stats_label} is length {length(stats_label)} and {.var risktable_stats} is length {length(risktable_stats)}."
      )
    )
  }

  if (!is.null(stats_label) && !rlang::is_named(stats_label)) {
    return(
      dplyr::tibble(
        stat_name = factor(risktable_stats, levels = risktable_stats),
        stat_label =
          unlist(stats_label) %>%
          factor(x = ., levels = rev(.))
      )
    )
  }

  dplyr::tibble(stat_name = factor(risktable_stats, levels = risktable_stats)) %>%
    dplyr::mutate(
      stat_label =
        risktable_stats %>%
        lapply(function(x) stats_label[[x]] %||% lst_stat_labels_default[[x]] %||% x) %>%
        unlist() %>%
        factor(x = ., levels = rev(.))
    )
}

# list of statistics and their default labels
lst_stat_labels_default <-
  list(
    n.risk = "At Risk",
    n.event = "Interval Events",
    n.censor = "Interval Censored",
    cum.event = "Events",
    cum.censor = "Censored"
  )

.create_list_of_gg_risk_tables <- function(df_times, risktable_stats, times,
                                           df_stat_labels, theme,
                                           risktable_group, color_block_mapping) {
  grouping_variable <-
    switch(risktable_group,
           "strata" = "strata",
           "risktable_stats" = "stat_label"
    )

  y_value <- c("strata", "stat_label") %>% setdiff(grouping_variable)
  if (y_value == "strata" && !"strata" %in% names(df_times)) {
    df_times$strata <- factor("Overall")
    df_times$strata_label <- factor("Overall")
  }

  df_risktable <-
    df_times %>%
    dplyr::select(dplyr::any_of(c("time", "strata", risktable_stats))) %>%
    tidyr::pivot_longer(
      cols = -dplyr::any_of(c("time", "strata")),
      names_to = "stat_name",
      values_to = "stat_value"
    ) %>%
    dplyr::mutate(
      stat_name = factor(.data$stat_name, levels = .env$risktable_stats)
    ) %>%
    dplyr::left_join(df_stat_labels, by = "stat_name")

  df_risktable %>%
    dplyr::mutate(
      "{y_value}" := factor(.data[[y_value]], levels = rev(levels(.data[[y_value]])))
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(grouping_variable))) %>%
    dplyr::group_map(
      function(data, df_group) {
        if (ncol(df_group) == 0L) {
          ggtitle_group_lbl <- list()
        } else {
          ggtitle_group_lbl <-
            list(ggplot2::ggtitle(dplyr::pull(df_group) %>% as.character()))
        }

        # construct the risktable ggplot
        gg <-
          ggplot2::ggplot(
            data,
            ggplot2::aes(
              x = .data$time,
              y = .data[[y_value]],
              label = .data$stat_value
            )
          ) +
          ggplot2::geom_text(size = 3.0, hjust = 0.5, vjust = 0.5, angle = 0, show.legend = FALSE)

        # apply styling to the plot
        gg +
          ggtitle_group_lbl +
          theme +
          switch(!is.null(color_block_mapping), .construct_color_block())
      }
    )
}

.prepare_data_for_risk_tables <- function(data, times, combine_groups) {
  df_times <-
    data %>%
    .add_tidy_times(times = times) %>%
    .add_cumulative_stats() %>%
    .keep_selected_times(times = times)

  # if isTRUE(combine_groups), combine all the stats across strata
  if (isTRUE(combine_groups)) {
    df_times <-
      df_times %>%
      dplyr::select(dplyr::any_of(c(
        "time", "n.risk",
        "n.event", "n.censor",
        "cum.event", "cum.censor"
      ))) %>%
      dplyr::group_by(.data$time) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c("n.risk", "n.event", "n.censor", "cum.event", "cum.censor")),
          sum
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
  }

  df_times
}




# # this is a near copy from visR, except the `scale_x_continuous() piece has been removed`
align_plots <- function(pltlist) {
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
