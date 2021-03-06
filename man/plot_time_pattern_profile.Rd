% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-time-pattern-profile.R
\name{plot_time_pattern_profile}
\alias{plot_time_pattern_profile}
\title{Plot average time pattern profiles.}
\usage{
plot_time_pattern_profile(data_tp, id = "all", ncol = 2, fluid = FALSE,
  myriad = TRUE)
}
\arguments{
\item{data_tp}{data frame containing questionnaire_id, kml3d results and time pattern data.}

\item{id}{time pattern to plot.}

\item{ncol}{facet columns.}

\item{fluid}{should be static bars or fluid lines visualized (boolean).}
}
\value{
ggplot2 avgerage time pattern profile plot.
}
\description{
Plot average time pattern profiles.
}
