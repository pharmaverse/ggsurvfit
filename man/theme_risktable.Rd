% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_risktable.R
\name{theme_risktable}
\alias{theme_risktable}
\alias{theme_risktable_default}
\alias{theme_risktable_boxed}
\title{Risk Table Themes}
\usage{
theme_risktable_default(axis.text.y.size = 10, plot.title.size = 10.75)

theme_risktable_boxed(axis.text.y.size = 10, plot.title.size = 10.75)
}
\arguments{
\item{axis.text.y.size}{text size of the labels on the left of the risk table}

\item{plot.title.size}{text size of the risk table title}
}
\value{
a ggplot2 figure
}
\description{
Returns ggplot list of calls defining a theme meant to be applied to a risk table.
}
\examples{
p <- survfit2(Surv(time, status) ~ 1, data = df_lung) \%>\%
  ggsurvfit() +
  scale_ggsurvfit()

# default ------------------------------------
p + add_risktable(theme = theme_risktable_default())

# larger text --------------------------------
p +
  add_risktable(
    size = 4,
    theme = theme_risktable_default(axis.text.y.size = 12,
                                    plot.title.size = 14)
  )

# boxed --------------------------------------
p + add_risktable(theme = theme_risktable_boxed())

# none ---------------------------------------
p + add_risktable(theme = NULL, risktable_height = 0.20)
}
