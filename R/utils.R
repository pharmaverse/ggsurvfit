.extract_formula_from_survfit <- function(x) {
  if (inherits(x, "survfitcox")) {
    x <-
      x$call %>%
      as.list() %>%
      `[[`("formula") %>%
      rlang::eval_tidy(env = x$.Environment) %>%
      utils::modifyList(val = list(.Environment = x$.Environment))
  }
  x$call %>%
    as.list() %>%
    `[[`("formula") %>%
    rlang::eval_tidy(env = x$.Environment)
}

.extract_data_from_survfit <- function(x) {
  if (inherits(x, "survfitcox")) {
    x <-
      x$call %>%
      as.list() %>%
      `[[`("formula") %>%
      rlang::eval_tidy(env = x$.Environment) %>%
      utils::modifyList(val = list(.Environment = x$.Environment))
  }
  x$call %>%
    as.list() %>%
    `[[`("data") %>%
    rlang::eval_tidy(env = x$.Environment)
}

.extract_arguments_from_attr <- function(x, attr_name) {
  lapply(x$layers, function(x) attr(x, attr_name, exact = TRUE)) %>%
    unlist(recursive = FALSE)
}


# this function assures that 5s are rounded up (and not to even, the default in `round()`)
round2 <- function(x, digits = 0) {
  round(x + .Machine$double.eps * sign(x), digits = digits)
}

allow_lambda <- function(x) {
  if (rlang::is_formula(x)) {
    rlang::as_function(x)
  } else {
    x
  }
}
