#' Calculate p-value
#'
#' @description
#' EXPERIMENTAL! Will likely change or perhaps even disappear.
#'
#' The function `survfit2_p()` wraps `survival::survdiff()` and returns
#' a formatted p-value.
#'
#' Function `logrank()` and `p_logrank()` are helper functions that
#' return the p-value from a the log-rank test with or without the "p" pre-pended.
#'
#' @param x a 'survfit2' object
#' @param pvalue_fun function to round and style p-value with
#' @param prepend_p prepend `"p="` to formatted p-value
#' @inheritDotParams survival::survdiff -data -formula
#'
#' @return a string
#' @name survfit2_p
#'
#' @examples
#' sf <- survfit2(Surv(time, status) ~ sex, data = df_lung)
#'
#' sf %>%
#'   ggsurvfit() +
#'   add_confidence_interval() +
#'   add_risktable() +
#'   ggplot2::labs(caption = glue::glue("Log-rank {p_logrank(sf)}"))
NULL

#' @export
#' @rdname survfit2_p
survfit2_p <- function(x, pvalue_fun = NULL, prepend_p = TRUE, ...) {
  if (!inherits(x, "survfit2")) {
    cli_abort(
      c("!" = "Argument {.code x} must be class {.cls survfit2},",
        "i" = "Create a {.cls survfit2} object with {.code survfit2()}.")
    )
  }

  # TODO: this needs to be UDPATED! can't use gtsummary
  pvalue_fun <- pvalue_fun %||% eval(rlang::parse_expr("gtsummary::style_pvalue"))

  survival::survdiff(
    formula = .extract_formula_from_survfit(x),
    data = .extract_data_from_survfit(x),
    ...
  ) %>%
    broom::glance() %>%
    dplyr::pull(.data$p.value) %>%
    pvalue_fun() %>%
    {dplyr::case_when(
      !prepend_p ~ .,
      prepend_p & startsWith(., "<") ~ paste0("p", .),
      prepend_p & !startsWith(., "<") ~ paste0("p=", .)
    )}
}

#' @export
#' @rdname survfit2_p
logrank <- function(x, pvalue_fun = NULL) {
  survfit2_p(x, pvalue_fun = pvalue_fun, prepend_p = FALSE, rho = 0)
}

#' @export
#' @rdname survfit2_p
p_logrank <- function(x, pvalue_fun = NULL) {
  survfit2_p(x, pvalue_fun = pvalue_fun, prepend_p = TRUE, rho = 0)
}
