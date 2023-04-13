#' Add risk table
#'
#' Add risk tables below the plot showing the number at risk, events observed, and
#' number of censored observations.
#'
#' @param times numeric vector of times where risk table values will be placed.
#' Default are the times shown on the x-axis. The times passed here will not
#' modify the tick marks shown on the figure. To modify which tick marks are
#' shown, use `ggplot2::scale_x_continuous(breaks=)`.
#' @param risktable_stats character vector of statistics to show in the risk table.
#' Must be one or more of `c("n.risk", "cum.event", "cum.censor", "n.event", "n.censor")`.
#' Default is `c("n.risk", "cum.event")`.
#' - `"n.risk"` Number of patients at risk
#' - `"cum.event"` Cumulative number of observed events
#' - `"cum.censor"` Cumulative number of censored observations
#' - `"n.event"` Number of events in time interval
#' - `"n.censor"` Number of censored observations in time interval
#'
#' See additional details below.
#' @param stats_label named vector or list of custom labels. Names are the statistics
#' from `risktable_stats=` and the value is the custom label.
#' @param risktable_group String indicating the grouping variable for the risk tables.
#' Default is `"auto"` and will select `"strata"` or `"risktable_stats"` based on context.
#' - `"strata"` groups the risk tables per stratum when present.
#' - `"risktable_stats"` groups the risk tables per risktable_stats.
#' @param combine_groups logical indicating whether to combine the statistics
#' in the risk table across groups. Default is `FALSE`
#' @param risktable_height A numeric value between 0 and 1 indicates the proportion of the
#' final plot the risk table will occupy.
#' @param theme A risk table theme. Default is `theme_risktable_default()`
#' @param size,... arguments passed to `ggplot2::geom_text(...)`. Pass arguments like, `size = 4`
#' to increase the size of the statistics presented in the table.
#'
#' @section Customize Statistics:
#' You can customize how the statistics in the risk table are displayed by
#' utilizing [glue](https://glue.tidyverse.org/)-like syntax in the `risktable_stats`
#' argument.
#'
#' For example, if you prefer to have the number at risk and the number of events
#' on the same row, you can use `risktable_stats = "{n.risk} ({cum.event})"`.
#'
#' You can further customize the table to include the risk estimates using
#' elements `c("estimate", "conf.low", "conf.high", "std.error")`. When using
#' these elements, you'll likely need to include a function to round the estimates
#' and multiply them by 100.
#'
#' ```r
#' add_risktable(
#'   risktable_stats =
#'     c("{n.risk} ({cum.event})",
#'       "{round(estimate*100)}% ({round(conf.low*100)}, {round(conf.high*100)})"),
#'   stats_label = c("At Risk (Cum. Events)", "Survival (95% CI)")
#' )
#' ```
#'
#' @section Competing Risks:
#'
#' The `ggcuminc()` can plot multiple competing events.
#' The `"cum.event"` and `"n.event"` statistics are the sum of all events across
#' outcomes _shown on the plot_.
#'
#' @export
#' @return a ggplot2 figure
#' @examples
#' p <-
#'   survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_censor_mark() +
#'   add_confidence_interval()
#'
#' # using the function defaults
#' p + add_risktable()
#'
#' # change the statistics shown and the label
#' p +
#'   add_risktable(
#'     risktable_stats = "n.risk",
#'     stats_label = list(n.risk = "Number at Risk"),
#'   )
#'
#' p +
#'   add_risktable(
#'     risktable_stats = "{n.risk} ({cum.event})"
#'   )
#'
#' p +
#'   add_risktable(
#'     risktable_stats = c("n.risk", "cum.event"),
#'     combine_groups = TRUE
#'   )
#' @inherit ggsurvfit seealso
add_risktable <- function(times = NULL,
                          risktable_stats = c("n.risk", "cum.event"),
                          risktable_group = c("auto", "strata", "risktable_stats"),
                          risktable_height = NULL,
                          stats_label = NULL,
                          combine_groups = FALSE,
                          theme = theme_risktable_default(),
                          size = 3.5,
                          ...) {

  add_risktable_empty_list <- list()

  rlang::inject(
    structure(add_risktable_empty_list,
              "add_risktable" =
                list(times = times,
                     risktable_stats = !!.convert_to_glue(risktable_stats),
                     stats_label = stats_label,
                     combine_groups = combine_groups,
                     risktable_group = match.arg(risktable_group),
                     risktable_height = risktable_height,
                     theme = theme,
                     !!!utils::modifyList(
                       x = list(size = size),
                       val = rlang::dots_list(...)
                     )
                ),
              class = "add_risktable")
  )
}

#' @export
ggplot_add.add_risktable <- function (object, plot, object_name) {
  update_add_risktable(plot, object)
}

update_add_risktable <- function(p, add_risktable_empty_list) {
  # confirm class and structure of object
  .is_ggsurvfit(p, fun_name = "add_rikstable()")
  p +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "points"),
      legend.margin = ggplot2::margin(t = 0, b = 0)
    ) +
    rlang::inject(
      structure(
        ggplot2::geom_blank(),
        add_risktable = !!attr(add_risktable_empty_list, "add_risktable")
      )
    )
}

.convert_to_glue <- function(x) {
  possible_stats <-
    c("n.risk", "n.event", "n.censor", "cum.event", "cum.censor",
      "estimate", "conf.low", "conf.high", "std.error")

  x[x %in% possible_stats] <- paste0("{", x[x %in% possible_stats], "}")

  x
}
