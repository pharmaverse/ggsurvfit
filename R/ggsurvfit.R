#' Plot Survival Probability
#'
#' Plot survival probabilities (and other transformations) using the results
#' from `survfit2()` or `survival::survfit()`; although, the former is recommend
#' to have the best experience with the **ggsurvfit** package.
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
#'   ggplot(aes(x = time, y = estimate,
#'              min = conf.low, ymax = conf.low,
#'              color = strata, fill = strata)) +
#'   geom_step()
#' ```
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' # Default publication ready plot
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   scale_ggsurvfit(x_scales = list(breaks = seq(0, 30, by = 6)))
#'
#' # Changing statistic type
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit(type = "cumhaz")
#'
#' # Configuring KM line type to vary by strata
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit(linetype_aes = TRUE) +
#'   scale_ggsurvfit()
#'
#' # Customizing the plot to your needs
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_censor_mark() +
#'   add_confidence_interval() +
#'   add_quantile() +
#'   add_risktable() +
#'   scale_ggsurvfit()
#' @seealso Visit the [gallery](https://www.danieldsjoberg.com/ggsurvfit/articles/gallery.html) for examples modifying the default figures
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
  if (inherits(x, "survfitms")) {
    cli_abort(c(
      "!" = "{.code ggsurvfit()} cannot be used to plot objects of class {.cls {class(x)}}.",
      "i" = "Use {.code ggcuminc()} for competing risks cumulative incidence plotting."
    ))
  }

  # prep data to be passed to ggplot() -----------------------------------------
  df <-
    tidy_survfit(x = x, type = type) %>%
    dplyr::filter(type != "cloglog" | .$time > 0) %>%
    dplyr::mutate(survfit = c(list(x), rep_len(list(), dplyr::n() - 1L)))

  # construct aes() call -------------------------------------------------------
  aes_args <- .construct_aes(df, linetype_aes = linetype_aes)

  # construction ggplot object -------------------------------------------------
  gg <- .construct_ggplot(x = x, df = df, aes_args = aes_args, theme = theme, ...)
  if (type == "cloglog") {
    gg <- gg + ggplot2::scale_x_log10()
  }

  # assign class and return object ---------------------------------------------
  class(gg) <- c("ggsurvfit", class(gg))

  gg
}

.construct_ggplot <- function(x, df, aes_args, theme, ...) {
  ggplot2::ggplot(data = df, ggplot2::aes(is_ggsurvfit = TRUE)) +
    list(
      rlang::inject(ggplot2::geom_step(ggplot2::aes(!!!aes_args), !!!rlang::dots_list(...))),
      ggplot2::labs(
        y = .default_y_axis_label(df),
        x = .default_x_axis_label(x),
        color = NULL,
        fill = NULL,
        linetype = NULL,
        alt = paste("Plot illustrating",
                    shQuote(.default_y_axis_label(df), type = "sh"),
                    "created with the 'ggsurvfit' R package.")
      ),
      theme
    )
}

# prepare `aes()` call
.construct_aes <- function(df, linetype_aes, outcome = NULL) {
  # if `linetype_aes` specified in an inappropriate situation, it is silently ignored
  if (
    isTRUE(linetype_aes) &&
    !(
      (is.null(outcome) && "strata" %in% names(df) && !isTRUE(getOption("ggsurvfit.switch-color-linetype"))) || # ggsurvfit with strata
      (!is.null(outcome) && length(outcome) == 1L && "strata" %in% names(df) && !isTRUE(getOption("ggsurvfit.switch-color-linetype"))) || # ggcuminc with 1 outcome and strata
      (!is.null(outcome) && length(outcome) > 1L && !"strata" %in% names(df) && isTRUE(getOption("ggsurvfit.switch-color-linetype"))) # ggcuminc with 2+ outcomes and no strata
    )
  ) {
    linetype_aes <- FALSE
  }


  # setting aes() --------------------------------------------------------------
  aes_args <-
    list(
      x = rlang::expr(.data$time),
      y = rlang::expr(.data$estimate)
    )

  # if a stratified model, add a `colour=` argument
  if ("strata" %in% names(df)) {
    aes_args <-
      c(
        aes_args,
        switch(
          getOption("ggsurvfit.switch-color-linetype", default = FALSE) %>%
            as.character(),
          "FALSE" = list(color = rlang::expr(.data$strata)),
          "TRUE" = list(linetype = rlang::expr(.data$strata))
        )
      )
  }

  # setting linetype -----------------------------------------------------------
  if (!is.null(outcome) && length(outcome) > 1) {
    aes_args <-
      c(
        aes_args,
        switch(
          getOption("ggsurvfit.switch-color-linetype", default = FALSE) %>%
            as.character(),
          "FALSE" = list(linetype = rlang::expr(.data$outcome)),
          "TRUE" = list(color = rlang::expr(.data$outcome))
        )
      )
  }

  # adding linetype_aes argument -----------------------------------------------
  if (isTRUE(linetype_aes) &&
      "strata" %in% names(df) &&
      !isTRUE(getOption("ggsurvfit.switch-color-linetype"))) {
    aes_args <-
      c(
        aes_args,
        switch(
          getOption("ggsurvfit.switch-color-linetype", default = FALSE) %>%
            as.character(),
          "FALSE" = list(linetype = rlang::expr(.data$strata)),
          "TRUE" = list(color = rlang::expr(.data$strata))
        )
      )
  }
  if (isTRUE(linetype_aes) &&
      !is.null(outcome) && length(outcome) > 1 &&
      isTRUE(getOption("ggsurvfit.switch-color-linetype"))) {
    aes_args <-
      c(
        aes_args,
        switch(
          getOption("ggsurvfit.switch-color-linetype", default = FALSE) %>%
            as.character(),
          "FALSE" = list(color = rlang::expr(.data$outcome)),
          "TRUE" = list(linetype = rlang::expr(.data$outcome))
        )
      )
  }

  aes_args
}

.is_ggsurvfit <- function(p, fun_name, required_cols = NULL) {
  if (
    !inherits(p, c("ggsurvfit", "ggcuminc")) ||
    (!is.null(required_cols) && any(!required_cols %in% names(ggplot2::ggplot_build(p)$plot$data)))
  ) {
    cli::cli_abort(c(
      "x" = "Cannot use {.code {fun_name}} in this context.",
      "i" = "Use {.code {fun_name}} after a call to {.code ggsurvfit()} or {.code ggcuminc()}"
    ))
  }

  return(invisible(TRUE))
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
    formula_lhs <- formula %>% rlang::f_lhs()
    data <- .extract_data_from_survfit(x)
  }
  else if (inherits(x, "tidycuminc")) {
    formula <- x$formula
    formula_lhs <- formula %>% rlang::f_lhs()
    data <- x$data
  } else {
    formula <- formula_lhs <- data <- NULL
  }

  # extract time variable ------------------------------------------------------
  if (!rlang::is_empty(formula_lhs) && !rlang::is_empty(all.vars(formula_lhs))) {
    time_variable <-
      formula_lhs %>%
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
  switch( # using the CDISC variable as default label, if present
    !is.null(data) && !is.null(formula) &&
      .is_CDISC_ADTTE(data) &&
      all(c("PARAM", "PARAMCD") %in% names(data)) &&
      !any(c("PARAM", "PARAMCD") %in% all.vars(formula)) &&
      .is_PARAM_consistent(formula, data),
    data[["PARAM"]] %>%
      unique() %>%
      paste(collapse = ", ")
  ) %||%
    switch(
      !is.null(time_variable) && !is.null(data) && time_variable %in% names(data),
      attr(data[[time_variable]], "label")
    ) %||%
    "Time"
}
