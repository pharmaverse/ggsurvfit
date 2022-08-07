#' Calculate p-value
#'
#' VERY EXPERIMENTAL! Will likely change or perhaps even disappear.
#'
#' @param x a 'survfit2' object
#' @param pvalue_fun function to round and style p-value with
#' @param prepend_p prepend `"p="` to formatted p-value
#' @inheritDotParams survival::survdiff -data -formula
#'
#' @return a string
#' @name survdiff2
#'
#' @examples
#' # add exmaple
NULL

#' @export
#' @rdname survdiff2
survdiff2 <- function(x, pvalue_fun = NULL, prepend_p = TRUE, ...) {
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
#' @rdname survdiff2
logrank <- function(x, pvalue_fun = NULL) {
  survdiff2(x, pvalue_fun = pvalue_fun, prepend_p = FALSE, rho = 0)
}

#' @export
#' @rdname survdiff2
p_logrank <- function(x, pvalue_fun = NULL) {
  survdiff2(x, pvalue_fun = pvalue_fun, prepend_p = TRUE, rho = 0)
}
