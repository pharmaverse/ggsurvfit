#' Add Quantile
#'
#' Add quantile information to the plot.
#'
#' @param y_value numeric value where the line segment will be drawn. Default is `0.5`
#' @param linetype Default is `"dashed"`. Argument is passed to `ggplot2::geom_segment()`
#' @inheritParams ggplot2::geom_segment
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_quantile(linetype = 2)
#'
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_quantile(linetype = 2) +
#'   add_quantile(y_value = 0.9, linetype = 3)
add_quantile <- function(y_value = 0.5, linetype = 2, ...) {
  ggplot2::layer(
    stat = StatQuantile, data = NULL, mapping = NULL, geom = "segment",
    position = "identity", show.legend = NA, inherit.aes = TRUE,
    params = list(
      na.rm = FALSE, y_value = y_value,
      linetype = linetype, ...
    ),
  )
}

StatQuantile <-
  ggplot2::ggproto(
    "StatQuantile",
    ggplot2::Stat,
    compute_panel =
      function(data, scales, params, y_value = 0.5) {
        .is_ggsurvfit(data, fun_name = "add_quantile()", required_aes_cols = c("x", "y"))
        quantile_km_in_stat(data, y_value)
      }
  )


quantile_km_in_stat <- function(data, y_value) {
  # create vertical line segments
  df_quantile <-
    data %>%
    .add_monotonicity_type() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("group"))) %>%
    dplyr::mutate(
      time_max = max(.data$x),
      quantile = .env$y_value,
      above_specified_quantile =
        .data$y >= .data$quantile & .data$x < .data$time_max
    ) %>%
    dplyr::ungroup() %>%
    tidyr::fill(.data$monotonicity_type, .direction = "updown") %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("group", "above_specified_quantile")))) %>%
    dplyr::mutate(
      row_number = dplyr::row_number(),
      rows_to_keep =
        (.data$monotonicity_type == "decreasing" & !.data$above_specified_quantile & dplyr::row_number() == 1L) |
          (.data$monotonicity_type == "increasing" & .data$above_specified_quantile & dplyr::row_number() == 1L)
    ) %>%
    dplyr::filter(.data$rows_to_keep) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$x, y = .data$quantile) %>%
    dplyr::mutate(xend = .data$x, yend = 0)

  # add row for horizontal line segment
  df_quantile %>%
    dplyr::bind_rows(
      dplyr::tibble(
        x = 0, y = y_value,
        xend = max(df_quantile$x), yend = y_value
      )
    )
}


.add_monotonicity_type <- function(x) {
  if ("monotonicity_type" %in% names(x)) {
    return(x)
  }

  x %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("group"))) %>%
    dplyr::mutate(
      monotonicity_type =
        dplyr::case_when(
          .data$y[1] > .data$y[dplyr::n()] ~ "decreasing",
          .data$y[1] < .data$y[dplyr::n()] ~ "increasing"
        )
    ) %>%
    dplyr::ungroup()
}
