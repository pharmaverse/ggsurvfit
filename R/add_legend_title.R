#' Add Legend Title
#'
#' Add a default or custom title to the figure legend.
#'
#' @param title a string to override the default legend title. Default is NULL
#'
#' @return a ggplot2 figure
#' @export
#'
#' @examples
#' survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
#'   ggsurvfit() +
#'   add_legend_title() +
#'   scale_ggsurvfit()
#' @inherit ggsurvfit seealso
add_legend_title <- function(title = NULL) {
  add_legend_title_empty_list <- list()
  structure(add_legend_title_empty_list, title = title, class = "add_legend_title")
}

#' @export
ggplot_add.add_legend_title <- function (object, plot, object_name) {
  update_add_legend_title(plot, object)
}

update_add_legend_title <- function(p, add_legend_title_empty_list) {
  # confirm class and structure of object
  .is_ggsurvfit(p, fun_name = "add_legend_title()")

  # extract legend title
  legend_title <-
    attr(add_legend_title_empty_list, "title") %||%
    p$data$strata_label[1]

  lst_labs <- list()
  p_build <- suppressWarnings(ggplot2::ggplot_build(p))

  # Get aesthetics that are actually mapped to variables 
  mapped_aes <- c()

  # Check main plot mapping for aesthetics mapped to actual variables
  for (aes_name in names(p$mapping)) {
    if (aes_name != "is_ggsurvfit") {  # Exclude internal ggsurvfit aesthetic
      mapped_aes <- c(mapped_aes, aes_name)
    }
  }

  # Check layer mappings for aesthetics mapped to actual variables
  for (layer in p$layers) {
    for (aes_name in names(layer$mapping)) {
      if (aes_name != "is_ggsurvfit") {  # Exclude internal ggsurvfit aesthetic
        mapped_aes <- c(mapped_aes, aes_name)
      }
    }
  }

  mapped_aes <- unique(mapped_aes)

  # Only set labels for aesthetics that are actually mapped
  if ("colour" %in% mapped_aes) {
    lst_labs[["colour"]] <- legend_title
  }
  if ("fill" %in% mapped_aes) {
    lst_labs[["fill"]] <- legend_title
  }
  # if there is a linetype aes() AND it's not from ggcuminc() with multiple outcomes, add title
  if ("linetype" %in% mapped_aes && (!inherits(p, "ggcuminc") || !length(unique(p$data$outcome)) > 1L)) {
    lst_labs[["linetype"]] <- legend_title
  }

  # apply labels and return updated figure
  ggplot2::update_labels(p, labels = lst_labs)
}