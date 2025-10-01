#' Calculate p-value
#'
#' The function `survfit2_p()` wraps `survival::survdiff()` and returns
#' a formatted p-value.
#'
#' @param x a 'survfit2' object
#' @param pvalue_fun function to round and style p-value with
#' @param prepend_p prepend `"p="` to formatted p-value
#' @param rho argument passed to  `survival::survdiff(rho=)`
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
survfit2_p <- function(x, pvalue_fun = format_p, prepend_p = TRUE, rho = 0) {
  if (!inherits(x, "survfit2")) {
    cli_abort(
      c("!" = "Argument {.code x} must be class {.cls survfit2},",
        "i" = "Create a {.cls survfit2} object with {.code survfit2()}.")
    )
  }
  if (inherits(x, "survfitms")) {
    cli_abort("The {.fun survfit2_p} does not support multi-state models.")
  }

  # call survdiff
  survdiff_args <-
    list(
      formula = .extract_formula_from_survfit(x),
      data = .extract_data_from_survfit(x),
      subset = as.list(x$call)[["subset"]],
      rho = rho
    ) %>%
    # remove NULL entries
    {Filter(Negate(is.null), x = .)} # styler: off

  survdiff_result <-
    do.call(
      what = survival::survdiff,
      args = survdiff_args,
      envir = x$.Environment
    )

  survdiff_result %>%
    broom::glance() %>%
    dplyr::pull("p.value") %>%
    pvalue_fun() %>%
    {dplyr::case_when(
      !prepend_p ~ .,
      prepend_p & grepl(pattern = "^<|^>", x = .) ~ paste0("p", .),
      prepend_p ~ paste0("p=", .)
    )}
}
