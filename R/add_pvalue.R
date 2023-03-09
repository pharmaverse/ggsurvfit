#' Add p-value
#'
#' @description
#' - `add_pvalue("caption")`: Add a p-value to the figure via `ggplot2::labs(caption=)`
#' - `add_pvalue("annotation")`: Add a p-value text annotation via `ggplot2::annotation("text")`
#'
#' P-values are calculated with `survival::survdiff()` or `tidycmprsk::glance()`.
#' Examples of custom placement located in the help file for `survfit_p()`.
#'
#' When a competing risks figure includes multiple outcomes, only the p-value
#' comparing stratum for the _first_ outcome can be placed.
#'
#' @param location string indicating where to place p-value. Must be one of
#' `c("caption", "annotation")`
#' @param caption string to be placed as the caption/annotation. String will
#' be processed with `glue::glue()`, and the default is `"{p.value}"`
#' @inheritParams survfit2_p
#' @inheritParams survival::survdiff
#' @param ... arguments passed to `ggplot2::annotate()`. Commonly used arguments
#' are `x=` and `y=` to place the p-value at the specified coordinates on the plot.
#'
#' @return a ggplot2 figure
#' @export
#'
#' @seealso `survfit_p()`
#' @examples
#' survfit2(Surv(time, status) ~ surg, df_colon) %>%
#'   ggsurvfit() +
#'   add_pvalue(caption = "Log-rank {p.value}")
#'
#' survfit2(Surv(time, status) ~ surg, df_colon) %>%
#'   ggsurvfit() +
#'   add_pvalue("annotation", size = 5)
#' @inherit ggsurvfit seealso
add_pvalue <- function(location = c("caption", "annotation"),
                       caption = "{p.value}",
                       prepend_p = TRUE,
                       pvalue_fun = format_p,
                       rho = 0,
                       ...) {
  add_pvalue_empty_list <- list()
  structure(add_pvalue_empty_list,
            location = match.arg(location),
            caption = caption,
            prepend_p = prepend_p,
            pvalue_fun = pvalue_fun,
            rho = rho,
            dots = rlang::dots_list(...),
            class = "add_pvalue")
}

#' @export
ggplot_add.add_pvalue <- function (object, plot, object_name) {
  update_add_pvalue(plot, object)
}

update_add_pvalue <- function(p, add_pvalue_empty_list) {
  .is_ggsurvfit(p, fun_name = "add_pvalue()")
  # getting user-passed arguments
  location <- attr(add_pvalue_empty_list, "location")
  caption <- attr(add_pvalue_empty_list, "caption")
  prepend_p <- attr(add_pvalue_empty_list, "prepend_p")
  pvalue_fun <- attr(add_pvalue_empty_list, "pvalue_fun")
  rho <- attr(add_pvalue_empty_list, "rho")
  dots <- attr(add_pvalue_empty_list, "dots")

  # checking inputs
  if (!rlang::is_string(caption)) {
    cli_abort(c("!" = "The {.code add_pvalue(caption=)} argument must be a string of length one."))
  }
  if (!rlang::is_function(pvalue_fun)) {
    cli_abort(c("!" = "The {.code add_pvalue(pvalue_fun=)} argument must be a function."))
  }
  if (!rlang::is_logical(prepend_p)) {
    cli_abort(c("!" = "The {.code add_pvalue(prepend_p=)} argument must be a logical."))
  }


  # extract survfit object
  build <- ggplot2::ggplot_build(p)
  survfit <- build$plot[["data"]][["survfit"]][[1]]

  if (!inherits(survfit, c("survfit2", "tidycuminc"))) {
    cli_inform(
      c("!" = "{.code add_pvalue()} works with objects created with {.code survfit2()} or {.code tidycmprsk::cuminc()}.",
        "i" = "{.code add_pvalue()} has been ignored.")
    )
    return(p)
  }
  if (inherits(survfit, "survfitcox")) {
    cli_inform(
      c("!" = "{.code add_pvalue()} does not support adjusted {.code coxph()} models.",
        "i" = "{.code add_pvalue()} has been ignored.")
    )
    return(p)
  }

  # calculate p-value
  if (inherits(survfit, "survfit2")) {
    p.value <- survfit2_p(survfit,
                          pvalue_fun = pvalue_fun,
                          prepend_p = prepend_p,
                          rho = rho)
  }
  else if (inherits(survfit, "tidycuminc")) {
    p.value <-
      tidycmprsk::glance(survfit) %>%
      dplyr::pull("p.value_1") %>%
      pvalue_fun() %>%
      {dplyr::case_when(
        !prepend_p ~ .,
        prepend_p & grepl(pattern = "^<|^>", x = .) ~ paste0("p", .),
        prepend_p ~ paste0("p=", .)
      )}
  }

  # add caption, and return ggplot
  if (location == "caption") {
    ret_object <-
      p +
      ggplot2::labs(caption = glue::glue(caption))
  }
  else if (location == "annotation") {
    location_args <-
      utils::modifyList(
        x = .default_pvalue_annotation_placement(build),
        val = dots %||% list()
      )

    ret_object <-
      p +
      rlang::inject(ggplot2::annotate("text", label = glue::glue(caption), !!!location_args))
  }
  ret_object

}


.default_pvalue_annotation_placement <- function(build) {
  x_range <- build$layout$panel_params[[1]]$x.range
  y_range <- build$layout$panel_params[[1]]$y.range

  x <- x_range[2] - diff(x_range) * 0.10

  if (!"monotonicity_type" %in% names(build$plot$data) ||
      build$plot$data$monotonicity_type[1] == "decreasing") {
    y <- y_range[2] - diff(y_range) * 0.10
  }
  else {
    y <- y_range[1] + diff(y_range) * 0.10
  }

  list(x = x, y = y)
}
