#' Calculate p-value
#'
#' The function `survfit2_p()` wraps `survival::survdiff()` and returns
#' a formatted p-value.
#'
#' @param x A [survfit2] object
#' @param format A bool or a function (formula would be okay) to round and style
#' p-value with. If `FALSE`, will return pvalue directly, if `TRUE`,
#' [survfit2_p] will be used to format the pvalue.
#' @param prepend_p A boolean value indicating whether a `"p="` should be
#' prepended to the formatted p-value. Only used when `format` is `TRUE` or a
#' function.
#' @param rho argument passed to  `survival::survdiff(rho=)`
#'
#' @return A number of pvalue if format is `FALSE`, otherwise, a string.
#' @name survfit2_p
#'
#' @examples
#' sf <- survfit2(Surv(time, status) ~ sex, data = df_lung)
#'
#' sf %>%
#'   ggsurvfit() +
#'   add_confidence_interval() +
#'   add_risktable() +
#'   scale_ggsurvfit() +
#'   labs(caption = glue::glue("Log-rank {survfit2_p(sf)}"))
#'
#' sf %>%
#'   ggsurvfit() +
#'   add_confidence_interval() +
#'   add_risktable() +
#'   scale_ggsurvfit() +
#'   annotate("text", x = 2, y = 0.05, label = glue::glue("{survfit2_p(sf)}"))
NULL

#' @export
#' @rdname survfit2_p
survfit2_p <- function(x, format = TRUE, prepend_p = TRUE, rho = 0) {
  if (!inherits(x, "survfit2")) {
    cli_abort(
      c(
        "!" = "Argument {.code x} must be class {.cls survfit2},",
        "i" = "Create a {.cls survfit2} object with {.code survfit2()}."
      )
    )
  }
  format <- allow_lambda(format)
  if (!(rlang::is_bool(format) || is.function(format))) {
    cli_abort("{.arg format} must be a bool or a {.cls function}")
  }
  out <- survival::survdiff(
    formula = .extract_formula_from_survfit(x),
    data = .extract_data_from_survfit(x),
    rho = rho
  ) %>%
    broom::glance() %>%
    dplyr::pull("p.value")
  if (isFALSE(format)) {
    return(out)
  }
  if (isTRUE(format)) format <- format_p
  out <- format(out)
  dplyr::case_when(
    !prepend_p ~ as.character(out),
    prepend_p & grepl(pattern = "^<|^>", x = out) ~ paste0("p", out),
    prepend_p ~ paste0("p=", out)
  )
}

#' Calculate Hazard ratio
#' @inheritParams survfit2_p
#' @inheritParams broom::tidy.coxph
#' @return An atomic numeric.
#' @examples
#' sf <- survfit2(Surv(time, status) ~ sex, data = df_lung)
#' survfit2_hr(sf)
#' survfit2_hr(sf, conf.int = TRUE)
#' survfit2_hr(sf, conf.int = TRUE, exponentiate = TRUE)
#' @export
survfit2_hr <- function(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, rho = 0) {
  if (!inherits(x, "survfit2")) {
    cli_abort(
      c(
        "!" = "Argument {.code x} must be class {.cls survfit2},",
        "i" = "Create a {.cls survfit2} object with {.code survfit2()}."
      )
    )
  }
  diff_res <- survival::survdiff(
    formula = .extract_formula_from_survfit(x),
    data = .extract_data_from_survfit(x),
    rho = rho
  )
  # https://www.sciencedirect.com/topics/medicine-and-dentistry/hazard-ratio
  obs1 <- diff_res$obs[1L]
  obs2 <- diff_res$obs[2L]
  exp1 <- diff_res$exp[1L]
  exp2 <- diff_res$exp[2L]
  estimate <- (obs2 / exp2) / (obs1 / exp1)
  ln_estimate <- log(estimate)
  if (exponentiate) {
    out <- estimate
  } else {
    out <- ln_estimate
  }
  if (!conf.int) {
    names(out) <- "estimate"
    return(out)
  }
  var_ln_estimate <- sqrt(1 / exp1 + 1 / exp2)
  tail <- (1 - conf.level) / 2L
  ci <- stats::qnorm(c(tail, 1L - tail), ln_estimate, var_ln_estimate)
  if (exponentiate) {
    ci <- exp(ci)
  }
  out <- c(out, ci)
  names(out) <- c("estimate", "ci.low", "ci.high")
  out
}
