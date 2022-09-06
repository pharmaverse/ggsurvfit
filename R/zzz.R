.onAttach <- function(libname, pkgname) {
  # https://stackoverflow.com/questions/53941218
  # ggplot2 has its own startup messages, and loading it like this supresses them (instead of just adding ggplot2 to depends)
  suppressPackageStartupMessages(attachNamespace("ggplot2"))
  packageStartupMessage("Loading required package: ggplot2")
}
