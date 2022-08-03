#' Plot survfit object
#'
#' @param x a survfit object
#' @param theme a survfit theme typically returned from `theme_ggsurvfit_survfit()`
#' @param ... arguments passed to `ggplot2::geom_step(...)`, e.g. `size = 2`
#' @inheritParams tidy_survfit
#'
#' @section Details:
#'
#' This function creates a ggplot figure from the 'survfit' object.
#' To better understand how to modify the figure, review the simplified
#' code used internally:
#'
#' ```{r eval = FALSE}
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   tidy_survfit() %>%
#'   ggplot(aes(x = time, y = estimate, y
#'              min = conf.low, ymax = conf.low,
#'              color = strata, fill = strata)) +
#'   geom_step()
#' ```
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit()
#'
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_censor_mark() +
#'   add_confidence_interval() +
#'   add_quantile() +
#'   add_risktable()
ggsurvfit <- function(x, type = "survival", theme = NULL, ...) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "survfit")) {
    cli_abort(
      c("!" = "Argument {.code x} must be {.cls survfit}.",
        "i" = "Create the object with {.code survfit2()}.")
    )
  }

  # prep data to be passed to ggplot() -----------------------------------------
  df <-  tidy_survfit(x = x, type = type)

  # construct aes() call -------------------------------------------------------
  aes_args <-
    list(
    x = rlang::expr(.data$time),
    y = rlang::expr(.data$estimate),
    is_ggsurvfit = TRUE
  )
  if ("strata" %in% names(df)) {
    aes_args <- c(aes_args, list(
      color = rlang::expr(.data$strata),
      fill = rlang::expr(.data$strata)
    ))
  }
  if ("conf.low" %in% names(df)) {
    aes_args <- c(aes_args, list(
      ymin = rlang::expr(.data$conf.low)
    ))
  }
  if ("conf.high" %in% names(df)) {
    aes_args <- c(aes_args, list(
      ymax = rlang::expr(.data$conf.high)
    ))
  }
  if ("n.censor" %in% names(df)) {
    aes_args <- c(aes_args, list(
      censor_count = rlang::expr(.data$n.censor)
    ))
  }

  # construction ggplot object -------------------------------------------------
  gg <-
    rlang::inject(ggplot2::ggplot(data = df, ggplot2::aes(!!!aes_args))) +
    list(
      ggplot2::geom_step(...),
      ggplot2::labs(
        y = .default_y_axis_label(type, df),
        x = .default_x_axis_label(x)
      ),
      switch("strata" %in% names(df),
        ggplot2::labs(
          color = df[["strata_label"]][1],
          fill = df[["strata_label"]][1]
        )
      ),
      theme
    )

  # assign class and return object ---------------------------------------------
  class(gg) <- c("ggsurvfit", class(gg))

  gg
}

.is_ggsurvfit <- function(x, fun_name, required_aes_cols = NULL) {
  if (
    (any(!c(required_aes_cols, "is_ggsurvfit") %in% names(x))) ||
    (!isTRUE(x$is_ggsurvfit[1]))
    ) {
    cli::cli_abort(c(
      "x" = "Cannot use {.code {fun_name}} in this context.",
      "i" = "Use {.code {fun_name}} after a call to {.code autofit.survfit()}"
    ))
  }

  return(invisible())
}

# function to assign default y-axis label from the statistic type
.default_y_axis_label <- function(type, df) {
  df[["estimate_type_label"]][1] %||% "estimate"
}

# function to assign default x-axis label from the survfit() object
.default_x_axis_label <- function(x) {
  if (inherits(x, "survfit2")) {
    time_variable <-
      .extract_formula_from_surfit(x) %>%
      all.vars() %>%
      `[`(1) %>%
      {
        switch(!is.na(.),
          .
        )
      } # convert NA to NULL, otherwise, return varname
    data <- .extract_data_from_surfit(x)
  } else {
    time_variable <- data <- NULL
  }

  x_label <-
    attr(data[[time_variable]], "label") %||%
    time_variable %||%
    "time"
}
