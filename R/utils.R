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

.extract_risktable_arguments <- function(x) {
  lapply(x$layers, function(x) attr(x, "risktable_args")) %>%
    unlist(recursive = FALSE)
}
