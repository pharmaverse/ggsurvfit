#' Compare KM curves
#'
#' Returns p-values calculated with `survival::survdiff()`
#'
#' @param x an object of class 'survfit'
#' @param test one of `c("logrank", "wilcoxon", "tarone_ware")`
#' @param rho Default is NULL. When not NULL, it's passed to `survdiff(rho=)` and
#' the `test=` argument is ignored.
#'
#' @return a p-value
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   survfit_pvalue(test = "logrank")
survfit_pvalue <- function(x, test = c("logrank", "wilcoxon", "tarone_ware"), rho = NULL) {
  if (!inherits(x, "survfit2")) {
    cli_abort("Argument {.var x} must be class {.cls survfit2}.")
  }
  if (is.character(test)) test <- match.arg(test)
  rho <-
    rho %||%
    switch(test,
      logrank = 0,
      wilcoxon = 1,
      tarone_ware = 1.5
    )

  # first extract needed item from survfit call
  call_list <- x$call %>% as.list()
  formula <-
    rlang::eval_tidy(
      call_list$formula,
      env = x$.Environment
    )
  data <-
    tryCatch(
      rlang::eval_tidy(
        call_list$data,
        env = x$.Environment
      ),
      error = function(e) {
        if (is.null(call_list$data)) {
          return(NULL)
        }
        msg <- "There was an error calculating the p-value."
        cli_abort(msg)
      }
    )

  survival::survdiff(formula = formula, data = data) %>%
    broom::glance() %>%
    `[[`("p.value")
}
