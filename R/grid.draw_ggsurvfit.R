#' Draw ggsurvfit object
#'
#' `grid::grid.draw()` methods for objects of classes 'ggsurvfit' and 'ggcuminc'.
#' These are implemented to allow users to directly call `ggplot2::ggsave()`
#' on 'ggsurvfit' figures.
#'
#' @param x an object of class 'ggsurvfit' or 'ggcuminc'
#' @inheritParams grid::grid.draw
#'
#' @return None
#' @name grid.draw_ggsurvfit
#' @keywords internal
#'
#' @examples
#' survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
#'   ggsurvfit() %>%
#'   grid.draw()
#'
#' library(tidycmprsk)
#' cuminc(Surv(ttdeath, death_cr) ~ trt, trial) %>%
#'   ggcuminc() %>%
#'   grid.draw()
NULL

#' @export
#' @rdname grid.draw_ggsurvfit
grid.draw.ggsurvfit <- function(x, recording = TRUE){
  ggsurvfit_build(x, combine_plots = TRUE) %>%
    grid::grid.draw(recording = recording)
}

#' @export
#' @rdname grid.draw_ggsurvfit
grid.draw.ggcuminc <- grid.draw.ggsurvfit
