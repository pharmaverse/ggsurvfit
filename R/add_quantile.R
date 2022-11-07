#' Add Quantile Annotation
#'
#' Add quantile information annotated on to the plot.
#'
#' @param y_value,x_value Numeric value where the line segment will be drawn.
#' Default is `y_value=0.5` when both `y_value` and `x_value` are unassigned.
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
#'
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_quantile(linetype = 2, y_value = NULL, x_value = 10)
add_quantile <- function(y_value = NULL, x_value = NULL, ...) {
  add_quantile_empty_list <- list()
  structure(add_quantile_empty_list,
            y_value = y_value,
            x_value = x_value,
            dots = utils::modifyList(x = list(linetype = 2, na.rm = FALSE),
                                     val = rlang::dots_list(...)),
            class = "add_quantile")
}

#' @export
ggplot_add.add_quantile <- function (object, plot, object_name) {
  update_add_quantile(plot, object)
}

update_add_quantile <- function(p, add_quantile_empty_list) {
  .is_ggsurvfit(p, fun_name = "add_quantile()", required_cols = c("time", "estimate"))
  # getting user-passed arguments
  y_value <- attr(add_quantile_empty_list, "y_value")
  x_value <- attr(add_quantile_empty_list, "x_value")
  dots <- attr(add_quantile_empty_list, "dots")

  if (is.null(y_value) && is.null(x_value)) y_value <- 0.5 # assign default value
  if (length(y_value) > 1 || length(x_value) > 1)
    cli_abort(c(
      "!" = "Neither {.code y_value} nor {.code x_value} may have length greater than one.",
      "i" = "To plot multiple quantiles, call {.code add_quantile()} multiple times."
    ))

  built_p <- ggplot2::ggplot_build(p)
  data <- built_p[["data"]][[1]]
  data$monotonicity_type <- suppressWarnings(built_p$plot$data$monotonicity_type[1])
  df_quantile_y <- .create_y_value_df(data, y_value)
  df_quantile_x <- .create_x_value_df(data, x_value)


  df_geom_segment <-
    dplyr::bind_rows(
      df_quantile_y,
      df_quantile_x
    )

  if (nrow(df_geom_segment) == 0L) return(p)

  p +
    rlang::inject(
      ggplot2::geom_segment(
        data = df_geom_segment,
        ggplot2::aes(
          x = .data$x, y = .data$y,
          xend = .data$xend, yend = .data$yend
        ),
        !!!dots
      )
    )
}

.create_x_value_df <- function(data, x_value) {
  if (is.null(x_value))
    return(
      dplyr::tibble(x = numeric(), y = numeric(),
                    xend = numeric(), yend = numeric())
    )

  # creating df with the horizontal line definition
  df_quantile <-
    data %>%
    dplyr::select(dplyr::all_of(c("x", "y", "group"))) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("group"))) %>%
    dplyr::mutate(
      x_max = max(.data$x)
    ) %>%
    dplyr::ungroup()

  # create tibble of times
  df_times <-
    dplyr::tibble(x = list(x_value), group = unique(data$group)) %>%
    tidyr::unnest(cols = "x")

  # merge tibble of times with tidy df
  df_quantile <-
    dplyr::full_join(
      df_quantile,
      df_times,
      by = c("x", "group")
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c("group", "x"))))

  # fill in missing stats
  df_quantile <-
    df_quantile %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("group"))) %>%
    tidyr::fill("y", "x_max", .direction = "down") %>%
    dplyr::ungroup()

  # any times above the max observed time are set to NA
  df_quantile <-
    df_quantile %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of("y"),
        ~ ifelse(.data$x > .data$x_max, NA, .)
      )
    ) %>%
    dplyr::filter(.data$x %in% .env$x_value) %>%
    dplyr::select(dplyr::all_of(c("x", "y"))) %>%
    dplyr::mutate(
      xend = 0,
      yend = .data$y
    ) %>%
    dplyr::filter(stats::complete.cases(.))

  # add row for vertical line segment
  if (nrow(df_quantile) > 0) {
    df_quantile <-
      df_quantile %>%
      dplyr::bind_rows(
        dplyr::tibble(
          x = x_value, y = 0,
          xend = x_value, yend = max(df_quantile$y)
        )
      )
  }

  df_quantile
}

.create_y_value_df <- function(data, y_value) {
  if (is.null(y_value))
    return(
      dplyr::tibble(x = numeric(), y = numeric(),
                    xend = numeric(), yend = numeric())
    )

  # create vertical line segments
  df_quantile <-
    data %>%
    .add_monotonicity_type() %>%
    dplyr::select(dplyr::any_of(c("x", "y", "group", "outcome", "monotonicity_type"))) %>%
    .add_requested_y_value(y_value = y_value) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("group", "outcome", "y")))) %>%
    dplyr::filter(.data$y %in% .env$y_value, dplyr::row_number() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select("x", "y") %>%
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
    {dplyr::arrange(
      .,
      !!!rlang::syms(intersect(c("group", "y"), names(.))),
      arrange_sign *.data$x # need to sort x based on the monotonicity of the curve
    )} %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("group"))) %>%
    tidyr::fill(
      "x",
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
    tidyr::drop_na("x") %>%
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
