#' Format p-value
#'
#' @param x numeric vector of p-values
#' @param digits number of digits large p-values will be rounded to.
#' Default is 2, and must be one of 1, 2, or 3.
#'
#' @return a string
#' @export
#'
#' @examples
#' p_vec <- c(0.00001, 0.01111, 0.0500000, 0.15, 0.99999)
#' format_p(p_vec)
#' format_p(p_vec, 2)
#' format_p(p_vec, 3)
format_p <- function(x, digits = 1) {
  # rounding large p-values to 1 digits
  if (digits == 1) {
    p_fmt <-
      dplyr::case_when(
        # allowing some leeway for numeric storage errors
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.9 ~ paste0(">", format_number(x = 0.9, digits = 1)),
        round(x, 1) >= 0.2 ~ format_number(x, digits = 1),
        round(x, 2) >= 0.1 ~ format_number(x, digits = 2),
        x >= 0.001 ~ format_number(x, digits = 3),
        x < 0.001 ~ paste0("<", format_number(x = 0.001, digits = 3))
      )
  }
  else if (digits == 2) {
    p_fmt <-
      dplyr::case_when(
        # allowing some leeway for numeric storage errors
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.99 ~ paste0(">", format_number(x = 0.99, digits = 2)),
        round2(x, 2) >= 0.1 ~ format_number(x, digits = 2),
        x >= 0.001 ~ format_number(x, digits = 3),
        x < 0.001 ~ paste0("<", format_number(x = 0.001, digits = 3))
      )
  }
  else if (digits == 3) {
    p_fmt <-
      dplyr::case_when(
        # allowing some leeway for numeric storage errors
        x > 1 + 1e-15 ~ NA_character_,
        x < 0 - 1e-15 ~ NA_character_,
        x > 0.999 ~ paste0(">", format_number(x = 0.999, digits = 3)),
        x >= 0.001 ~ format_number(x, digits = 3),
        x < 0.001 ~ paste0("<", format_number(x = 0.001, digits = 3))
      )
  }
  else {
    stop("The `digits=` argument must be 1, 2, or 3.")
  }

  p_fmt
}

format_number <- function(x, digits = 0) {
  round2(x, digits = digits) %>%
    format(nsmall = digits, scientific = FALSE, trim = TRUE)
}

round2 <- function(x, digits = 0) {
  round(x + .Machine$double.eps * sign(x), digits = digits)
}

