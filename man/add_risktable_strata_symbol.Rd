% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_risktable_strata_symbol.R
\name{add_risktable_strata_symbol}
\alias{add_risktable_strata_symbol}
\title{Use Symbol for Strata in Risk Table}
\usage{
add_risktable_strata_symbol(
  symbol = NULL,
  size = 15,
  face = "bold",
  vjust = 0.3,
  ...
)
}
\arguments{
\item{symbol}{\href{https://en.wikipedia.org/wiki/UTF-8}{UTF-8 code} of shape to
replace strata level with. Default is a rectangle (\code{"\\U25AC"}).
Other common options are circle (\code{"\\U25CF"}) and diamond (\code{"\\U25C6"}).
While a symbol is the most common string to pass here, any string is acceptable.}

\item{size, face, vjust, ...}{arguments passed to a function similar to
\code{ggtext::element_markdown()}}
}
\value{
a ggplot2 figure
}
\description{
Replace the stratum level names with a color symbol in the risk tables.
Use this function when stratum level names are long.
}
\examples{
\dontshow{if (!names(grDevices::dev.cur()) \%in\% c("pdf", "postscript")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
p <-
  survfit2(Surv(time, status) ~ sex, data = df_lung) \%>\%
  ggsurvfit(linewidth = 1) +
  add_confidence_interval() +
  add_risktable(risktable_group = "risktable_stats") +
  scale_ggsurvfit()

 p + add_risktable_strata_symbol()
 p + add_risktable_strata_symbol(symbol = "\U25CF", size = 10)
\dontshow{\}) # examplesIf}
}
\seealso{
Visit the \href{https://www.danieldsjoberg.com/ggsurvfit/articles/gallery.html}{gallery} for examples modifying the default figures
}
