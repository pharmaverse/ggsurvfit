.onAttach <- function(libname, pkgname) {
  # https://stackoverflow.com/questions/53941218
  # ggplot2 has its own startup messages, and loading it like this suppresses them (instead of just adding ggplot2 to depends)
  # wrapping in tryCatch to avoid error when it's already attached
  tryCatch(
    {
      suppressPackageStartupMessages(attachNamespace("ggplot2"))
      packageStartupMessage("Loading required package: ggplot2")
    },
    error = function(e) invisible()
  )
}
