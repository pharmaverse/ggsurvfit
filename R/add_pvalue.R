#' Add p-value
#'
#' @description
#' - `add_pvalue_caption()`: Add a p-value to the figure via `ggplot2::labs(caption=)`
#' - `add_pvalue_annotation()`: Add a p-value text annotation via `ggplot2::annotation("text")`
#'
#' P-values are calculated with `survival::survdiff()`.
#' Examples of custom placement located in the help file for `survfit_p()`.
#'
#' @param caption string to be placed as the caption/annotation. String will
#' be processed with `glue::glue()`, and the default is "{p.value}"
#' @inheritParams survfit2_p
#' @inheritParams survival::survdiff
#' @param ... arguments passed to `ggplot2::annotate()`
#'
#' @return a ggplot2 figure
#' @name add_pvalue
#'
#' @seealso `survfit_p()`
#' @examples
#' survfit2(Surv(time, status) ~ surg, df_colon) %>%
#'   ggsurvfit() +
#'   add_pvalue_caption()
#'
#' survfit2(Surv(time, status) ~ surg, df_colon) %>%
#'   ggsurvfit() +
#'   add_pvalue_annotation(x = 1, y = 0.4)
NULL

#' @export
#' @rdname add_pvalue
add_pvalue_caption <- function(caption = "{p.value}",
                               prepend_p = TRUE,
                               pvalue_fun = format_p,
                               rho = 0) {
  rlang::inject(
    ggplot2::layer(
      data = NULL, mapping = NULL,
      stat = StatBlankSurvfit, geom = "blank",
      position = "identity",
      show.legend = NA, inherit.aes = TRUE,
      params = list()
    ) %>%
      structure(
        "add_pvalue_caption" = list(caption = caption,
                                    pvalue_fun = pvalue_fun,
                                    prepend_p = prepend_p,
                                    rho = rho,
                                    pvalue_type = "caption")
      )
  )
}

#' @export
#' @rdname add_pvalue
add_pvalue_annotation <- function(caption = "{p.value}",
                                  prepend_p = TRUE,
                                  pvalue_fun = format_p,
                                  rho = 0,
                                  ...) {
  rlang::inject(
    ggplot2::layer(
      data = NULL, mapping = NULL,
      stat = StatBlankSurvfit, geom = "blank",
      position = "identity",
      show.legend = NA, inherit.aes = TRUE,
      params = list()
    ) %>%
      structure(
        "add_pvalue_annotation" = list(caption = caption,
                                       pvalue_fun = pvalue_fun,
                                       prepend_p = prepend_p,
                                       rho = rho,
                                       pvalue_type = "annotation", ...)
      )
  )
}



.add_pvalue_caption <- function(object, caption, pvalue_fun, prepend_p, pvalue_type, rho, ...) {
  # extract survfit object
  build <- ggplot2::ggplot_build(object)
  survfit <- build$data[[1]][["survfit"]][[1]]

  # calculate p-value
  p.value <- survfit2_p(survfit,
                        pvalue_fun = pvalue_fun,
                        prepend_p = prepend_p,
                        rho = rho)

  # add caption, and return ggplot
  if (pvalue_type == "caption") {
    ret_object <-
      object +
      ggplot2::labs(caption = glue::glue(caption))
  }
  else if (pvalue_type == "annotation") {
    location_args <-
      modifyList(
        x = .default_pvalue_annotation_placement(build),
        val = rlang::dots_list(...)
      )

    ret_object <-
      object +
      rlang::inject(ggplot2::annotate("text", label = glue::glue(caption), !!!location_args))
  }
  ret_object
}

.default_pvalue_annotation_placement <- function(build) {
  x_range <- build$layout$panel_params[[1]]$x.range
  y_range <- build$layout$panel_params[[1]]$y.range

  x <- x_range[2] - diff(x_range) * 0.10

  if (!"monotonicity_type" %in% names(build$data[[1]]) ||
      build$data[[1]]$monotonicity_type[1] == "decreasing") {
    y <- y_range[2] - diff(y_range) * 0.10
  }
  else {
    y <- y_range[1] + diff(y_range) * 0.10
  }

  list(x = x, y = y)
}
