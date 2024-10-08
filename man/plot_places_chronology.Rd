% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-places-chronology.R
\name{plot_places_chronology}
\alias{plot_places_chronology}
\title{Plot single or multiple place chronologies in different scales.}
\usage{
plot_places_chronology(
  data,
  id,
  weekday = "all",
  size_range = NULL,
  colour_path = "black",
  size_path = 2,
  alpha_path = 0.25,
  alpha_points = 0.85,
  linetype_path = "solid",
  force_repel = 3,
  title = NULL,
  axis_label = FALSE,
  xlim = NULL,
  ylim = NULL,
  xextra = 3,
  print_place_duration = TRUE,
  point_padding = unit(1, "lines"),
  exclude_sleep = TRUE,
  facets = FALSE
)
}
\arguments{
\item{data}{a data frame (columns: ID, day, duration, place, address, lon, lat, prop_duration).}

\item{id}{vector, which contains questionnaire ids. Choosa "all" to compute all ids.}

\item{weekday}{vector, which contains the weekday to plot.}

\item{size_range}{specify the size for visualizatipn of duration.}

\item{colour_path}{sepcify the path line colour.}

\item{size_path}{specify the path line size.}

\item{alpha_path}{specify the path line alpha value [0:1].}

\item{alpha_points}{specify the point alpha value [0:1].}

\item{linetype_path}{specify the linetype of the path line.}

\item{force_repel}{specify how heavy the repel algorithmn should be.}

\item{title}{title of the plot.}

\item{axis_label}{show or hide axis labels (boolean).}

\item{xlim}{specify plot x limits.}

\item{ylim}{specify plot y limits.}

\item{xextra}{extra space for time plot (units in cm).}

\item{print_place_duration}{print place overall duration (hours).}

\item{point_padding}{Amount of padding around labeled point. Defaults to unit(0, "lines").}

\item{exclude_sleep}{exclude sleep duration (boolean).}

\item{facets}{plot facets (boolean).}
}
\value{
ggplot2 visualization of place chronology data.
}
\description{
Plot single or multiple place chronologies in different scales.
}
