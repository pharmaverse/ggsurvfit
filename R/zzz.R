.onLoad <- function(...) {
  tryCatch(
    registerS3method(
      "knit_print", "ggsurvfit",
      "knit_print.ggsurvfit",
      envir = getNamespace("knitr")
    ),
    warning = function(w) invisible(),
    error = function(e) invisible()
  )
}
