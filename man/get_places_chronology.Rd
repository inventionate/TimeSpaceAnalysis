% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-places-chronology.R
\name{get_places_chronology}
\alias{get_places_chronology}
\title{Reshape place chronology data.}
\usage{
get_places_chronology(data, id = "all", weekday = "all", title,
  exclude_sleep)
}
\arguments{
\item{data}{a data frame, which contains place chronology data.}

\item{id}{vector, which contains questionnaiere ids.}

\item{weekday}{vector, which contains a day selection.}

\item{title}{specify plot title.}

\item{exclude_sleep}{exclude sleep duration (boolean).}
}
\value{
reshaped data frame for further visualization.
}
\description{
Reshape place chronology data.
}
