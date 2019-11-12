#' @include utilities.R
#' @include add-theme.R
#' @include get-gda-trajectory.R
NULL
#' Visualization of trajectories (connected active and passive individual points).
#'
#' @param res_gda MCA result (rownames have to be questionnaire IDs including time number, e.g. 87654_1).
#' @param select vector of names, within_inertia of individuals selection (within_inertia: vector containing the number of high variation and low variationindividuals) or case (vector containing NULL, complete, or incomplete).
#' @param axes axes to plot.
#' @param open_sans use Open Sans font (boolean).
#' @param labels plot individual labels (boolean).
#' @param title the plot title
#' @param ind_labels plot labels (boolean).
#' @param time_point_names vector containing the name of the time points.
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param axis_lab_name name of axis label.
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#' @param axes_annotate_alpha alpha value of axes annotations.
#' @param legend_x x position of legend.
#' @param legend_y y position of legend.
#'
#' @return trajectory ggplot2 visualization.
#' @export
fviz_gda_trajectory <- function(res_gda,
                                select = list(name = NULL, within_inertia = NULL, case = NULL),
                                title = NULL,
                                axes = 1:2,
                                ind_labels = FALSE,
                                open_sans = TRUE,
                                time_point_names = NULL,
                                plot_modif_rates = TRUE,
                                axis_lab_name = "Achse",
                                labels = NULL,
                                legend_x = 0.12,
                                legend_y = 0.9,
                                axes_annotate_alpha = 0.3) {

  # Add Open Sans font family
  if (open_sans) .add_fonts()

  # Evaluate axes
  axis_1 <- sym(paste0("Dim.", axes[1]))
  axis_2 <- sym(paste0("Dim.", axes[2]))

  # Trajektoriedaten zusammenstellen
  coord_trajectory <- get_gda_trajectory(res_gda, time_point_names)
  coord_all <-  coord_trajectory$coord_all
  time_point_names <- coord_trajectory$time_point_names

  # Auswahl vornehmen
  selected_ind <- .select_trajectory(coord_all, select, time_point_names, axes)

  # Filterung vornehmen
  coord_ind_timeseries <-
    coord_all %>%
    filter(id %in% selected_ind$id)

  # Plot der Daten
  if (inherits(res_gda, c("MCA"))) {
    p <- .create_plot()
  } else {
    stop("Only MCA plots are currently supported!")
  }

  p <-
    p +
    scale_colour_brewer(palette = "YlGnBu", direction = -1) +
    geom_point(
      data = coord_ind_timeseries,
      aes(
        !!axis_1,
        !!axis_2
      ),
      colour = "black",
      size = 4
    ) +
    geom_point(
      data = coord_ind_timeseries,
      aes(
        !!axis_1,
        !!axis_2,
        colour = time
      ),
      size = 2.5
    ) +
    geom_path(
      data = coord_ind_timeseries,
      aes(
        !!axis_1,
        !!axis_2,
        group = id
      ),
      size = 1,
      arrow = arrow(
        length = unit(0.3, "cm"),
        type = "closed"
      )
    )

  # Labeln
  if (ind_labels) {
    p <-
      p +
      ggrepel::geom_label_repel(
        data = coord_ind_timeseries %>%
          filter(time == time_point_names[1]),
        aes(
          !!axis_1,
          !!axis_2,
          colour = time,
          label = id
        )
      )
  }

  # Beschriftung anpassen
  p <- .finalize_plot(p, res_gda, axes, labels)

  p <- .annotate_axes(p, labels, alpha = axes_annotate_alpha)

  if (!is_null(title)) p <- p + ggtitle(title)

  # Beschreibung der Punkte
  p <-
    p +
    theme(
      plot.title = element_blank(),
      legend.position = "bottom",
      legend.box.background = element_rect(
        linetype = "solid",
        colour = "gray17",
        fill = "white"
      ),
      legend.text = element_text(size = 10),
      legend.box.margin = margin(0, 0.2, 0.1, 0, "cm"),
      legend.title = element_blank(),
      legend.direction = "horizontal"
    ) +
    guides(
      colour = guide_legend(
        override.aes = list(size = 4))
    )

  # Plotten
  p
}
