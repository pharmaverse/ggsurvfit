.extract_formula_from_survfit <- function(x) {
  x$call %>%
    as.list() %>%
    `[[`("formula") %>%
    rlang::eval_tidy(env = x$.Environment)
}

.extract_data_from_survfit <- function(x) {
  x$call %>%
    as.list() %>%
    `[[`("data") %>%
    rlang::eval_tidy(env = x$.Environment)
}

.extract_arguments_from_attr <- function(x, attr_name) {
  lapply(x$layers, function(x) attr(x, attr_name, exact = TRUE)) %>%
    unlist(recursive = FALSE)
}

