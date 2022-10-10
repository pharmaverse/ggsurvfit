#' Add Legend Title
#'
#' Add a default or custom title to the figure legend.
#'
#' @param title a string to override the default legend title. Default is `NULL`
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
#'   ggsurvfit() +
#'   add_legend_title()
add_legend_title <- function(title = NULL) {
  add_legend_title_empty_list <- list()
  structure(add_legend_title_empty_list, title = title, class = "add_legend_title")
}

#' @export
ggplot_add.add_legend_title <- function (object, plot, object_name) {
  update_add_legend_title(plot, object)
}

update_add_legend_title <- function(p, add_legend_title_empty_list) {
  if (!is.null(attr(add_legend_title_empty_list, "title")) &&
      !rlang::is_string(attr(add_legend_title_empty_list, "title"))) {
    cli::cli_abort(c("!" = "The {.code add_legend_title(title=)} argument must be a string."))
  }
  legend_title <-
    attr(add_legend_title_empty_list, "title") %||%
    p$data$strata_label[1]

  lst_labs <- list()
  p_build <- ggplot2::ggplot_build(p)

  # if colour or fill present add the title for those aes()
  if ("colour" %in% names(p_build$data[[1]])) {
    lst_labs[["colour"]] <- legend_title
  }
  if ("fill" %in% names(p_build$data[[1]])) {
    lst_labs[["fill"]] <- legend_title
  }
  # if there is a linetype aes() AND it's not from ggcuminc() with multiple outcomes, add title
  if ("linetype" %in% names(p_build$data[[1]]) && (!inherits(p, "ggcuminc") || !length(unique(p$data$outcome)) > 1L)) {
    lst_labs[["linetype"]] <- legend_title
  }

  # apply labels and return updated figure
  ggplot2::update_labels(p, labels = lst_labs)
}
