#' Add Censor Marking
#'
#' @param ... arguments passed to passed to
#' `ggplot2::geom_point(...)` with defaults `shape = 3` and `size = 2`
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_confidence_interval() +
#'   add_censor_mark()
add_censor_mark <- function(...) {
  rlang::inject(
    ggplot2::layer(
      stat = StatCensorMark, data = NULL, mapping = NULL, geom = "point",
      position = "identity", show.legend = NA, inherit.aes = TRUE,
      params =
          utils::modifyList(x = list(na.rm = TRUE, size = 2, shape = 3),
                            val = rlang::dots_list(...))
    )
  )
}

StatCensorMark <-
  ggplot2::ggproto(
    "StatCensorMark",
    ggplot2::Stat,
    compute_panel =
      function(data, scales, params) {
        .is_ggsurvfit(data, fun_name = "add_censor_mark()", required_aes_cols = c("x", "y", "censor_count"))
        .compute_censor_data(data)
      }
  )

.compute_censor_data <- function(x) {
  # expanding censored rows (removing rows with 0 censored obs)
  tidyr::uncount(x, weights = .data$censor_count)
}
