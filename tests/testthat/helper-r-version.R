# we only want to run vdiffr snapshot tests on the latest version of R,
# because there are sometimes changes across all R versions/OSs
current_release_version <-
  tryCatch(
    read.dcf(url("https://cran.r-project.org/src/base/VERSION-INFO.dcf"))[1,][1] |>
      unname(),
    error = function(e) character(0L)
  )

loaded_release_version <-
  paste0(R.version$major, ".", R.version$minor)
