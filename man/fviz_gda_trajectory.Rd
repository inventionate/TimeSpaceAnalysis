% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fviz-gda-trajectory.R
\name{fviz_gda_trajectory}
\alias{fviz_gda_trajectory}
\title{Visualization of trajectories (connected active and passive individual points).}
\usage{
fviz_gda_trajectory(
  res_gda,
  select = list(name = NULL, within_inertia = NULL, case = NULL),
  title = NULL,
  axes = 1:2,
  ind_labels = FALSE,
  time_point_names = NULL,
  plot_modif_rates = TRUE,
  axis_lab_name = "Achse",
  labels = NULL,
  legend_x = 0.12,
  legend_y = 0.9,
  axes_annotate_alpha = 0.3,
  xlim = NULL,
  ylim = NULL
)
}
\arguments{
\item{res_gda}{MCA result (rownames have to be questionnaire IDs including time number, e.g. 87654_1).}

\item{select}{vector of names, within_inertia of individuals selection (within_inertia: vector containing the number of high variation and low variationindividuals) or case (vector containing NULL, complete, or incomplete).}

\item{title}{the plot title}

\item{axes}{axes to plot.}

\item{ind_labels}{plot labels (boolean).}

\item{time_point_names}{vector containing the name of the time points.}

\item{plot_modif_rates}{plot modified rates instead of eigenvalue percentage (boolean).}

\item{axis_lab_name}{name of axis label.}

\item{labels}{label axes (vector of length 4; left, right, top, bottom).}

\item{legend_x}{x position of legend.}

\item{legend_y}{y position of legend.}

\item{axes_annotate_alpha}{alpha value of axes annotations.}

\item{xlim}{x Axis limits (vector of length 2).}

\item{ylim}{y Axis limits (vector of length 2).}
}
\value{
trajectory ggplot2 visualization.
}
\description{
Visualization of trajectories (connected active and passive individual points).
}
