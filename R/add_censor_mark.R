#' Add Censor Marking
#'
#' @param shape,size,... arguments passed to passed to
#' `ggplot2::geom_point(shape, size, ...)`
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_confidence_interval() +
#'   add_censor_mark()
add_censor_mark <- function(shape = 3, size = 2, ...) {
  ggplot2::layer(
    stat = StatCensorMark, data = NULL, mapping = NULL, geom = "point",
    position = "identity", show.legend = NA, inherit.aes = TRUE,
    params = list(na.rm = TRUE, size = size, shape = shape, ...),
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
