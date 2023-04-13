# code taken from response on SO https://stackoverflow.com/questions/74072102

setOldClass("ggplot")
setClass("ggsurvfit", contains = "ggplot", slots = c(p = "ggplot"))

#' @export
setMethod("|", c("ggsurvfit", "ggsurvfit"),
          function(e1, e2) {
            x1 <- build_and_wrap(e1@p)
            x2 <- build_and_wrap(e2@p)
            get("|.ggplot", asNamespace("patchwork"), mode = "function")(x1, x2)
          })
# TODO: Add the other combinations of classes,
#       e.g. c("ggsurvfit", "ggplot"), c("ggplot", "ggsurvfit"), etc

# this function does the final build of the objects (ie adds the risk tables),
# then wraps the result in patchwork::wrap_elements() as needed to prep it
# to be passed to the patchwork methods
build_and_wrap <- function(x, wrap_elements = TRUE) {
  browser()
  if (inherits(x, c("ggsurvfit", "ggcuminc"))) {
    x <- ggsurvfit_build(x)
  }
  if (isTRUE(wrap_elements) && inherits(x, "ggsurvfit_build")) {
    x <- patchwork::wrap_elements(x)
  }
  x
}
