#' Plot Survival Probability
#'
#' @param theme a survfit theme. Default is `theme_ggsurvfit_default()`
#' @param linetype_aes logical indicating whether to add `ggplot2::aes(linetype = strata)`
#' to the `ggplot2::geom_step()` call. When strata are present, the resulting figure
#' will be a mix a various line types for each stratum.
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
#' @return a ggplot2 figure
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
ggsurvfit <- function(x, type = "survival",
                      linetype_aes = FALSE,
                      theme = theme_ggsurvfit_default(), ...) {
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
  aes_args <- .construct_aes(df, linetype_aes = linetype_aes)

  # construction ggplot object -------------------------------------------------
  gg <- .construct_ggplot(x = x, df = df, aes_args = aes_args, theme = theme, ...)

  # assign class and return object ---------------------------------------------
  class(gg) <- c("ggsurvfit", class(gg))

  gg
}

.construct_ggplot <- function(x, df, aes_args, theme, ...) {
  rlang::inject(ggplot2::ggplot(data = df, ggplot2::aes(!!!aes_args))) +
    list(
      ggplot2::geom_step(...),
      ggplot2::labs(
        y = .default_y_axis_label(df),
        x = .default_x_axis_label(x),
        alt = paste("Plot illustrating",
                    shQuote(.default_y_axis_label(df), type = "sh"),
                    "created with the 'ggsurvfit' R package.")
      ),
      switch("strata" %in% names(df),
             ggplot2::labs(
               color = NULL,
               fill = NULL,
               linetype = NULL
             )
      ),
      theme
    )
}

.construct_aes <- function(df, linetype_aes) {
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
  if (isTRUE(linetype_aes) && "strata" %in% names(df)) {
    aes_args <- c(aes_args, list(
      linetype = rlang::expr(.data$strata)
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
.default_y_axis_label <- function(df) {
  df[["estimate_type_label"]][1] %||% "estimate"
}

# function to assign default x-axis label from the survfit() object
.default_x_axis_label <- function(x) {
  # extract formula and data ---------------------------------------------------
  if (inherits(x, "survfit2")) {
    formula <- .extract_formula_from_survfit(x)
    data <- .extract_data_from_survfit(x)
  }
  else if (inherits(x, "tidycuminc")) {
    formula <- x$formula
    data <- x$data
  } else {
    formula <- data <- NULL
  }

  # extract time variable ------------------------------------------------------
  if (!is.null(formula)) {
    time_variable <-
      formula %>%
      all.vars() %>%
      `[`(1) %>%
      {
        switch(!is.na(.),
               .
        )
      } # convert NA to NULL, otherwise, return varname
  } else {
    time_variable <- NULL
  }

  # return time label ----------------------------------------------------------
  attr(data[[time_variable]], "label") %||%
    time_variable %||%
    "time"
}
