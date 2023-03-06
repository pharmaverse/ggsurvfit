#' Create survival curves
#'
#' @description
#' Simple wrapper for `survival::survfit.formula()` except the environment is also
#' included in the returned object.
#'
#' Use this function with all other functions in this package to ensure
#' all elements are calculable.
#'
#' @inheritParams survival::survfit.formula
#' @inheritDotParams survival::survfit.formula
#'
#' @section `survfit2()` vs `survfit()`:
#'
#' Both functions have identical inputs, so why do we need `survfit2()`?
#'
#' The *only* difference between `survfit2()` and `survival::survfit()` is that the
#' former tracks the environment from which the call to the function was made.
#'
#' The definition of `survfit2()` is unremarkably simple:
#'
#' ```r
#' survfit2 <- function(formula, ...) {
#'   # construct survfit object
#'   survfit <- survival::survfit(formula, ...)
#'
#'   # add the environment
#'   survfit$.Environment = <calling environment>
#'
#'   # add class and return
#'   class(survfit) <- c("survfit2", "survfit")
#'   survfit
#' }
#' ```
#'
#' The environment is needed to ensure the survfit call can be accurately
#' reconstructed or parsed at any point post estimation.
#' The call is parsed when p-values are reported and when labels are created.
#' For example, the raw variable names appear in the output of a stratified
#' `survfit()` result, e.g. `"sex=Female"`. When using `survfit2()`, the
#' originating data frame and formula may be parsed and the raw variable
#' names removed.
#'
#' Most functions in the package work with both `survfit2()` and `survfit()`;
#' however, the output will be styled in a preferable format with `survfit2()`.
#'
#' @return survfit2 object
#' @export
#'
#' @seealso [`survival::survfit.formula()`]
#' @examples
#' # With `survfit()`
#' fit <- survfit(Surv(time, status) ~ sex, data = df_lung)
#' fit
#'
#' # With `survfit2()`
#' fit2 <- survfit2(Surv(time, status) ~ sex, data = df_lung)
#' fit2
#'
#' # Consistent behavior with other functions
#' summary(fit, times = c(10, 20))
#'
#' summary(fit2, times = c(10, 20))
#'
survfit2 <- function(formula, ...) {
  if (missing(formula)) {
    cli::cli_abort("The {.code formula} argument cannot be missing.")
  }
  if (!rlang::is_formula(formula) & !inherits(formula, "coxph")) {
    cli::cli_abort(
      c("x" = "The {.code formula} argument must be class {.cls formula} or {.cls coxph}.",
        "i" = "Argument is class {.cls {class(formula)}}")
    )
  }

  # create call to `survfit()` -------------------------------------------------
  # solution taken from https://adv-r.hadley.nz/evaluation.html#match.call
  call <- match.call(survival::survfit, expand.dots = TRUE)
  call[[1]] <- quote(survival::survfit)

  # evaluate call --------------------------------------------------------------
  survfit <- eval(call, parent.frame())

  # checking if data was piped in with magrittr --------------------------------
  if (lapply(as.list(call), function(x) identical(x, quote(.))) %>% unlist() %>% any()) {
    # save the "dot" to the new environment, so it can be evaluated later in functions like `survfti2_p()`
    env <- rlang::env(parent.frame(), `.` = eval(quote(.), parent.frame()))
  }
  else env <- parent.frame()

  # update object with env and add another class -------------------------------
  survfit %>%
    utils::modifyList(val = list(.Environment = env)) %>%
    structure(class = c("survfit2", class(survfit))) %>%
    .check_PARAM_consistency()
}

.check_PARAM_consistency <- function(x) {
  # get the formula and data
  formula <- .extract_formula_from_survfit(x)
  data <- .extract_data_from_survfit(x)

  if (is.null(data) || is.null(formula)) return(x)
  if (.is_CDISC_ADTTE(data) && !.is_PARAM_consistent(formula, data))
    cli::cli_warn(c("!" = "Columns {.cls {c('PARAM', 'PARAMCD')}} are not unique and usage is likely incorrect."))

  x
}

.is_CDISC_ADTTE <- function(data) {
  all(c("AVAL", "CNSR") %in% names(data)) &&
    any(c("PARAM", "PARAMCD") %in% names(data))
}

.is_PARAM_consistent <- function(formula, data) {
  isTRUE(
    (
      # PARAM and PARAMCD both present with appropriate lengths
      (
        all(c("PARAM", "PARAMCD") %in% names(data)) &&
          (length(unique(data[["PARAM"]])) == 1L) &&
          (length(unique(data[["PARAMCD"]])) == 1L)
      ) ||
        # PARAMCD only present, and is appropriate length
        (
          !"PARAM" %in% names(data) &&
            "PARAMCD" %in% names(data) &&
            (length(unique(data[["PARAMCD"]])) == 1L)
        )
    ) ||
      # or PARAM can be any length and it must appear in formula
      (any(c("PARAM", "PARAMCD") %in% all.vars(formula)))
  )
}
