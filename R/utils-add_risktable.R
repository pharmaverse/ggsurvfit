# this function returns a combined primary plot with risktables below.
.construct_risktable <- function(x, times, risktable_stats, stats_label, group,
                                 combine_groups, risktable_group,
                                 risktable_height, theme, combine_plots,
                                 risktable_symbol_args, ...) {
  # build the ggplot to inspect the internals ----------------------------------
  plot_build <- ggplot2::ggplot_build(x)

  # if plot is faceted, return plot without risktable --------------------------
  if (.is_faceted(plot_build)) {
    return(structure(x, class = setdiff(class(x), c("ggsurvfit", "ggcuminc"))))
  }

  # get data to place in risktables --------------------------------------------
  times <- times %||% plot_build$layout$panel_params[[1]]$x.sec$breaks
  df_times <-
    .prepare_data_for_risk_tables(data = x$data, times = times, combine_groups = combine_groups)


  # determine grouping if not specified ----------------------------------------
  if (risktable_group == "auto") {
    risktable_group <-
      dplyr::case_when(
        "strata" %in% names(df_times) & length(risktable_stats) == 1L ~ "risktable_stats",
        TRUE ~ "strata"
      )
  }

  # determine risktable height -------------------------------------------------
  risktable_height <-
    .calculate_risktable_height(risktable_height, risktable_group, risktable_stats, df_times)

  # create list of ggplots, one plot for each risktable ------------------------
  df_stat_labels <- .construct_stat_labels(risktable_stats, stats_label)

  gg_risktable_list <-
    .create_list_of_gg_risk_tables(
      df_times, risktable_stats, times,
      df_stat_labels, theme, risktable_group,
      color_block_mapping =
        .match_strata_level_to_color(plot_build, risktable_group, risktable_symbol_args),
      risktable_symbol_args = risktable_symbol_args,
      ...
    )

  # align all the plots --------------------------------------------------------
  gg_risktable_list_aligned <-
    c(list(x), gg_risktable_list) %>%
    align_plots()

  # combine all plots into single figure ---------------------------------------
  if (isFALSE(combine_plots)) return(gg_risktable_list_aligned)

  risktable_n <- length(gg_risktable_list_aligned) - 1
  gg_final <-
    gg_risktable_list_aligned %>%
    patchwork::wrap_plots(
      ncol = 1,
      heights =
        c(1 - risktable_height,
          rep_len(risktable_height / risktable_n, length.out = risktable_n))
    )

  gg_final
}

.calculate_risktable_height <- function(risktable_height, risktable_group, risktable_stats, df_times) {
  if (!is.null(risktable_height)) return(risktable_height)


  if (risktable_group == "strata" & !"strata" %in% names(df_times))
    group_n <- 1
  else if (risktable_group == "strata" & "strata" %in% names(df_times))
    group_n <- length(unique(df_times[["strata"]]))
  else if (risktable_group == "risktable_stats")
    group_n <- length(risktable_stats)


  if (risktable_group == "risktable_stats" & !"strata" %in% names(df_times))
    stat_n <- 1
  else if (risktable_group == "risktable_stats" & "strata" %in% names(df_times))
    stat_n <- length(unique(df_times[["strata"]]))
  else if (risktable_group == "strata")
    stat_n <- length(risktable_stats)

  -0.07383 +
      0.03500 * (group_n == 1 & risktable_group == "risktable_stats") +
      0.09950 * group_n +
      0.01949 * stat_n +
      0.03547 * as.numeric(group_n > 1) +
      0.01517 * group_n * stat_n
}

# this function returns a named list
# with the labels for the stats in the risktable
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


# returns a list of the risktable ggplot objects
.create_list_of_gg_risk_tables <- function(df_times, risktable_stats, times,
                                           df_stat_labels, theme,
                                           risktable_group,
                                           color_block_mapping,
                                           risktable_symbol_args, ...) {
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

  geom_text_args <-
    list(size = 2.85, hjust = 0.5, vjust = 0.5, angle = 0, show.legend = FALSE) %>%
    utils::modifyList(val = rlang::dots_list(...))

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
          rlang::inject(ggplot2::geom_text(!!!geom_text_args))

        # apply styling to the plot
        gg +
          ggtitle_group_lbl +
          theme +
          switch(
            !rlang::is_empty(risktable_symbol_args),
            rlang::inject(.construct_color_block(color_block_mapping, !!!risktable_symbol_args))
          )
      }
    )
}

.combine_over_outcomes <- function(x) {
  # only need to combine over 'outcomes' if multiple outcomes shown in figure
  if (!"outcome" %in% names(x) || length(unique(x$outcome)) < 2) return(x)
  x %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("time", "strata")))) %>%
    dplyr::summarise(
      n.risk = getElement(.data$n.risk, 1),
      n.event = sum(.data$n.event),
      n.censor = getElement(.data$n.censor, 1),
      cum.event = sum(.data$cum.event),
      cum.censor = getElement(.data$cum.censor, 1)
    ) %>%
    dplyr::ungroup()
}

.combine_over_strata <- function(x, combine_groups) {
  # if isTRUE(combine_groups), combine all the stats across strata
  if (!isTRUE(combine_groups)) return(x)

  x %>%
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

# creating a data frame with the stats needed for the risk tables
.prepare_data_for_risk_tables <- function(data, times, combine_groups) {
  df_times <-
    data %>%
    .add_tidy_times(times = times) %>%
    .add_cumulative_stats() %>%
    .keep_selected_times(times = times) %>%
    .combine_over_outcomes() %>%
    .combine_over_strata(combine_groups = combine_groups)

  df_times
}



# this function takes each individual plot and
# returns a list of ggplots that have been modified so they can be stacked
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


# function returns T/F whether or not the plot has been faceted
.is_faceted <- function(ggbuild) {
  if (!inherits(ggbuild$plot$facet, "FacetNull")) {
    c("i" = "The {.code add_risktable()} function is not compatible with a faceted ggplot.") %>%
      cli_inform()
    return(TRUE)
  }
  FALSE
}
