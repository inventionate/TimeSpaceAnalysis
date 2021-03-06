% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/designate-axes.R
\name{designate_axes}
\alias{designate_axes}
\title{Designate axes parts.}
\usage{
designate_axes(x = NULL, y = NULL, designation = c("left/bottom",
  "right/top"), rotate = FALSE, size = 5, fontface = "bold.italic",
  alpha = 1)
}
\arguments{
\item{x}{coordinate of the axes designation. The value will automatically mirrored.}

\item{y}{coordinate of the axes designation. The value will automatically mirrored.}

\item{designation}{character vector with the two designation titles.}

\item{rotate}{designate x (FALSE) or y axis (TRUE).}

\item{size}{size of the designation titles.}

\item{fontface}{fontface.}

\item{alpha}{font alpha.}
}
\value{
ggplot2 annotations.
}
\description{
Designate axes parts.
}
