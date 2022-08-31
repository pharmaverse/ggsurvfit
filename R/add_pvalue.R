#' Add p-value
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
  survfit <- ggplot2::ggplot_build(object)$data[[1]][["survfit"]][[1]]

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
    ret_object <-
      object +
      ggplot2::annotate("text", label = glue::glue(caption), ...)
  }
  ret_object
}
