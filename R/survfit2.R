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
#' @return surfit2 object
#' @export
#'
#' @seealso [`survival::survfit.formula()`]
#' @examples
#' survfit2(Surv(time, status) ~ sex, data = df_lung)
survfit2 <- function(formula, ...) {
  if (missing(formula)) {
    cli::cli_abort("The {.code formula} argument cannot be missing.")
  }
  if (!rlang::is_formula(formula)) {
    cli::cli_abort(
      c("x" = "The {.code formula} argument must be class {.cls formula}.",
        "i" = "Argument is class {.cls {class(formula)}}")
    )
  }
  survfit <- survival::survfit(formula = formula, ...)

  # update object with env and add another class
  survfit %>%
    utils::modifyList(val = list(.Environment = rlang::current_env())) %>%
    structure(class = c("survfit2", class(survfit)))
}
