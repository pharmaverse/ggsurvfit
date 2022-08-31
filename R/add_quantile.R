#' Add Quantile Annotation
#'
#' Add quantile information annotated on to the plot.
#'
#' @param y_value Numeric value where the line segment will be drawn. Default is `0.5`
#' @param ... Named arguments passed to `ggplot2::geom_segment()` with default `linetype = 2`
#'
#' @return a ggplot2 figure
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
add_quantile <- function(y_value = 0.5, ...) {
  ggplot2::layer(
    stat = StatQuantileSurvfit, data = NULL, mapping = NULL, geom = "segment",
    position = "identity", show.legend = NA, inherit.aes = TRUE,
    params =
      c(
        list(y_value = y_value),
        utils::modifyList(x = list(linetype = 2, na.rm = FALSE),
                          val = rlang::dots_list(...))
      )
  )
}

StatQuantileSurvfit <-
  ggplot2::ggproto(
    "StatQuantileSurvfit",
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
    dplyr::select(dplyr::any_of(c("x", "y", "group", "outcome", "monotonicity_type"))) %>%
    .add_requested_y_value(y_value = y_value) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("group", "outcome", "y")))) %>%
    dplyr::filter(.data$y %in% .env$y_value, dplyr::row_number() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$x, .data$y) %>%
    dplyr::mutate(xend = .data$x, yend = 0)

  # add row for horizontal line segment
  if (nrow(df_quantile) > 0) {
    df_quantile <-
      df_quantile %>%
      dplyr::bind_rows(
        dplyr::tibble(
          x = 0, y = y_value,
          xend = max(df_quantile$x), yend = y_value
        )
      )
  }

  df_quantile
}

.add_requested_y_value <- function(data, y_value) {
  monotonicity_type <- data$monotonicity_type[1]
  arrange_sign <-
    dplyr::case_when(
      monotonicity_type == "decreasing" ~ -1L,
      monotonicity_type == "increasing" ~ 1L
    )

  data %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("group"))) %>%
    dplyr::mutate(
      y_extreme =
        dplyr::case_when(
          .env$monotonicity_type == "decreasing" ~ min(.data$y, na.rm = TRUE),
          .env$monotonicity_type == "increasing" ~ max(.data$y, na.rm = TRUE)
        )
    ) %>%
    {dplyr::rows_upsert(
      .,
      dplyr::select(., dplyr::any_of(c("group", "monotonicity_type", "y_extreme"))) %>%
        dplyr::distinct() %>%
        dplyr::mutate(y = .env$y_value),
      by = intersect(c("group", "y"), names(.))
    )} %>%
    # {dplyr::arrange(., !!!rlang::syms(intersect(c("group", "y"), names(.))), dplyr::desc(.data$x))} %>%
    {dplyr::arrange(
      .,
      !!!rlang::syms(intersect(c("group", "y"), names(.))),
      arrange_sign *.data$x # need to sort x based on the monotonicity of the curve
    )} %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("group"))) %>%
    tidyr::fill(
      .data$x,
      .direction =
        dplyr::case_when(
          monotonicity_type == "decreasing" ~ "down",
          monotonicity_type == "increasing" ~ "up"
        )
    ) %>%
    dplyr::mutate(
      x =
        dplyr::case_when(
          .data$monotonicity_type == "decreasing" ~
            ifelse(.data$y < .data$y_extreme, NA, .data$x),
          .data$monotonicity_type == "increasing" ~
            ifelse(.data$y > .data$y_extreme, NA, .data$x)
        )
    ) %>%
    tidyr::drop_na(.data$x) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("group", "x", "y"))))
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
