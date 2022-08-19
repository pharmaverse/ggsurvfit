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
#' @param ... arguments passed to `ggplot2::geom_text(...)`. Pass arguments like, `size = 3`
#' to increase the size of the statistics presented in the table.
#'
#' @export
#' @return a ggplot2 figure
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
                          risktable_height = NULL,
                          stats_label = NULL,
                          combine_groups = FALSE,
                          theme = theme_risktable_default(),
                          ...) {
  rlang::inject(
    ggplot2::layer(
      data = NULL, mapping = NULL,
      stat = StatBlankSurvfit, geom = "blank",
      position = "identity",
      show.legend = NA, inherit.aes = TRUE,
      params = list()
    ) %>%
      structure(
        "add_risktable" = list(
          times = times,
          risktable_stats =
            !!match.arg(
              rev(risktable_stats),
              choices = c("n.risk", "cum.censor", "cum.event", "n.censor", "n.event"),
              several.ok = TRUE
            ),
          stats_label = stats_label,
          combine_groups = combine_groups,
          risktable_group = !!match.arg(risktable_group),
          risktable_height = risktable_height,
          theme = theme,
          ...
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

