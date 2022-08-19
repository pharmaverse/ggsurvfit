#' Add Confidence Interval
#'
#' Add a confidence interval represented by either a ribbon or lines.
#'
#' @param type string indicating the type of confidence interval to draw.
#' Must be one of `c("ribbon", "lines")`
#' @param ... arguments pass to geom.
#' - `type = 'ribbon'`: Defaults are `ggplot2::geom_ribbon(alpha = 0.2, color = NA, ...)`
#' - `type = 'lines'`: Defaults are `ggplot2::geom_step(linetype = "dashed", na.rm = TRUE, ...)`
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ sex, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_confidence_interval()
#'
#' survfit2(Surv(time, status) ~ 1, data = df_lung) %>%
#'   ggsurvfit() +
#'   add_confidence_interval(type = "lines")
add_confidence_interval <- function(type = c("ribbon", "lines"), ...) {
  rlang::inject(!!ci_geom_selector(type = match.arg(type), ...))
}

ci_geom_selector <- function(type, ...) {
  geom_args <-
    switch(type,
           "ribbon" = list(na.rm = TRUE, alpha = 0.2, color = NA),
           "lines" = list(na.rm = TRUE, linetype = "dashed")
    ) %>%
    utils::modifyList(val = rlang::dots_list(...))

  switch(
    type,
    "ribbon" =
      rlang::inject(
          ggplot2::layer(
            data = NULL, mapping = NULL,
            stat = StatConfidenceInterval, geom = "ribbon",
            position = "identity",
            show.legend = NA, inherit.aes = TRUE,
            params = list(direction = "hv", !!!geom_args)
          )
      ),
    "lines" =
      rlang::inject(
        list(
          ggplot2::layer(
            stat = StatStepLow, data = NULL, mapping = NULL, geom = "step",
            position = "identity", show.legend = NA, inherit.aes = TRUE,
            params = !!geom_args,
          ),
          ggplot2::layer(
            stat = StatStepHigh, data = NULL, mapping = NULL, geom = "step",
            position = "identity", show.legend = NA, inherit.aes = TRUE,
            params = !!geom_args,
          )
        )
      )
  )
}

# this is a copy of StatStepribbon, but includes a data check to ensure it was created by this pkg
StatConfidenceInterval <-
  ggplot2::ggproto(
    "StatConfidenceInterval", ggplot2::Stat,
    compute_group = function(data, scales, direction = "hv",
                             yvars = c("ymin", "ymax"), ...) {
      .is_ggsurvfit(data, fun_name = "add_confidence_interval()", required_aes_cols = c("x", "ymin", "ymax"))
      stairstepn(data = data, direction = direction, yvars = yvars)
    }
  )



StatStepLow <-
  ggplot2::ggproto(
    "StatStepLow",
    ggplot2::Stat,
    compute_panel =
      function(data, scales, params) {
        .is_ggsurvfit(data, fun_name = "add_confidence_interval()", required_aes_cols = c("x", "ymin"))
        data %>% dplyr::mutate(y = .data$ymin)
      }
  )

StatStepHigh <-
  ggplot2::ggproto(
    "StatStepHigh",
    ggplot2::Stat,
    compute_panel =
      function(data, scales, params) {
        .is_ggsurvfit(data, fun_name = "add_confidence_interval()", required_aes_cols = c("x", "ymax"))
        data %>% dplyr::mutate(y = .data$ymax)
      }
  )
