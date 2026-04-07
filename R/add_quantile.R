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
#'   add_quantile(linetype = 2) +
#'   scale_ggsurvfit()
#'
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_quantile(linetype = 2) +
#'   add_quantile(y_value = 0.9, linetype = 3) +
#'   scale_ggsurvfit()
#'
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_quantile(linetype = 2, y_value = NULL, x_value = 10) +
#'   scale_ggsurvfit()
#' @inherit ggsurvfit seealso
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
ggplot_add.add_quantile <- function (object, plot, ...) {
  update_add_quantile(plot, object)
}

update_add_quantile <- function(p, add_quantile_empty_list) {
  # confirm class and structure of object
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

  built_p <- suppressWarnings(ggplot2::ggplot_build(p))
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

.find_quantile_x <- function(
  x,
  y,
  y_value,
  x_max,
  monotonicity_type,
  tolerance = sqrt(.Machine$double.eps)
) {
  if (monotonicity_type == "decreasing") {
    # x1: first time y drops to or below y_value
    idx1 <- which(y <= y_value + tolerance)
    if (length(idx1) == 0L) return(NA_real_)
    x1 <- x[idx1[1]]

    # x2: first time y drops strictly below y_value
    idx2 <- which(y < y_value - tolerance)
    if (length(idx2) == 0L) {
      # plateau runs to end of follow-up
      return((x1 + x_max) / 2)
    }
    x2 <- x[idx2[1]]

    if (x2 > x1 + tolerance) {
      # genuine plateau at y_value: use midpoint
      return((x1 + x2) / 2)
    }
    # no plateau: curve steps through y_value immediately
    return(x1)
  } else {
    # increasing
    # x1: first time y rises to or above y_value
    idx1 <- which(y >= y_value - tolerance)
    if (length(idx1) == 0L) return(NA_real_)
    x1 <- x[idx1[1]]

    # x2: first time y rises strictly above y_value
    idx2 <- which(y > y_value + tolerance)
    if (length(idx2) == 0L) {
      return((x1 + x_max) / 2)
    }
    x2 <- x[idx2[1]]

    if (x2 > x1 + tolerance) {
      return((x1 + x2) / 2)
    }
    return(x1)
  }
}

.create_y_value_df <- function(data, y_value) {
  if (is.null(y_value))
    return(
      dplyr::tibble(x = numeric(), y = numeric(),
                    xend = numeric(), yend = numeric())
    )

  # Determine monotonicity type from data
  # Use column if it exists; otherwise derive via .add_monotonicity_type()
  # Note: monotonicity_type may not be present when .create_y_value_df() is
  # called directly with tidy_survfit() output (e.g. in unit tests)
  mono_type <- if ("monotonicity_type" %in% names(data)) {
    data[["monotonicity_type"]][1]
  } else {
    NA_character_
  }
  if (is.na(mono_type) || is.null(mono_type)) {
    mono_type <- .add_monotonicity_type(data, estimate_var = "y")[[
      "monotonicity_type"
    ]][1]
  }

  # Determine groups
  # When group strata not present, treat data as one group
  has_group <- "group" %in% names(data)
  if (has_group) {
    groups <- unique(data[["group"]])
  } else {
    groups <- list(NULL) # single sentinel value => process all rows once
  }

  # Compute the quantile x-value per group using helper
  results <- lapply(groups, function(g) {
    if (has_group) {
      grp_data <- data[data[["group"]] == g, ]
    } else {
      grp_data <- data
    }
    grp_data <- grp_data[order(grp_data[["x"]]), ]
    x_max <- max(grp_data[["x"]], na.rm = TRUE)
    qx <- .find_quantile_x(
      x = grp_data[["x"]],
      y = grp_data[["y"]],
      y_value = y_value,
      x_max = x_max,
      monotonicity_type = mono_type
    )
    if (!is.na(qx)) {
      dplyr::tibble(x = qx, y = y_value)
    } else {
      dplyr::tibble(x = numeric(), y = numeric())
    }
  })

  df_quantile <- dplyr::bind_rows(results)

  if (nrow(df_quantile) == 0L) {
    return(
      dplyr::tibble(
        x = numeric(),
        y = numeric(),
        xend = numeric(),
        yend = numeric()
      )
    )
  }

  # Create vertical line segments (from each quantile point down to y = 0)
  df_quantile <- df_quantile %>%
    dplyr::mutate(xend = .data$x, yend = 0)

  # Add horizontal line segment (from x = 0 across to the rightmost quantile)
  df_quantile <-
    df_quantile %>%
    dplyr::bind_rows(
      dplyr::tibble(
        x = 0,
        y = y_value,
        xend = max(df_quantile$x),
        yend = y_value
      )
    )

  df_quantile
}
