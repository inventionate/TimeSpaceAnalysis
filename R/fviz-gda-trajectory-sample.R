#' @include utilities.R
#' @include add-theme.R
#' @include get-gda-trajectory.R
NULL
#' Visualization of the separated concentration ellipses of the sample.
#'
#' @param res_gda MCA result (rownames have to be questionnaire IDs including time number, e.g. 87654_1).
#' @param time_point_names vector containing the name of the time points.
#' @param axes the axes to plot.
#' @param ind_points show individuals (boolean).
#' @param title title of the plot
#' @param concentration_ellipse show or hide overall concentration ellipse (boolean).
#' @param complete plot only complete cases (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param alpha ellipse fill alpha.
#' @param axis_lab_name name of axis label.
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#' @param axes_annotate_alpha alpha value of axes annotations.
#' @param legend_x x position of legend.
#' @param legend_y y position of legend.
#' @param xlim x Axis limits (vector of length 2).
#' @param ylim y Axis limits (vector of length 2).
#'
#' @return ggplot2 visualization.
#' @export
fviz_gda_trajectory_sample <- function(res_gda, time_point_names = NULL, axes = 1:2, ind_points = TRUE,
                                       concentration_ellipse = TRUE, complete = TRUE, title = NULL,
                                       plot_modif_rates = TRUE, alpha = 0.15, axis_lab_name = "Achse",
                                       axes_annotate_alpha = 0.3, labels = NULL, legend_x = 0.12,
                                       legend_y = 0.9, xlim = NULL, ylim = NULL) {

  # Evaluate axes
  axis_1 <- sym(paste0("Dim.", axes[1]))
  axis_2 <- sym(paste0("Dim.", axes[2]))

  # Trajektoriedaten zusammenstellen
  coord_trajectory <- get_gda_trajectory(res_gda, time_point_names)
  coord_all <-  coord_trajectory$coord_all
  time_point_names <- coord_trajectory$time_point_names

  if (complete) {
    # Auswahl, falls nur komplette Fälle
    selected_ind <-
      .select_trajectory(
        coord_all,
        select = list(case = "complete"),
        time_point_names, axes
      )

    # Filterung vornehmen
    coord_all <-
      coord_all %>%
      filter(id %in% selected_ind$id)

  }

  # Mittelpunkte
  coord_mean <-
    coord_all %>%
    select(-id) %>%
    group_by(time) %>%
    summarise_all(mean)

  # Massen der Individuenmittelpunkte
  coord_mass <-
    coord_all %>%
    select(-id) %>%
    count(time) %>%
    rename(mass = n)

  # Mittelpunkte und Massen zusammenführen
  coord_mean_mass <- full_join(coord_mean, coord_mass, by = "time")

  # Standardisierte Distanzen hinzufügen
  gda_dist <- function(dim, res_gda, coord) {
      dist_make(
          coord[dim] %>% as.matrix(),
          function (v1, v2) abs((v1 - v2)/sqrt(res_gda$eig$eigenvalue[dim]))
      )
  }

  coord <- coord_mean_mass %>% select(-time)

  dim_n <- length(axes)

  dims <- setNames(1:dim_n, paste0("Dim.", 1:dim_n))

  dist <-
      dims %>%
      map(~ gda_dist(., res_gda, coord))

  message("Distanzen der Schwerpunkte (SD):")
  print(dist)

  # Masse hinzufügen
  coord_all <-
    coord_all %>%
    select(
      !!axis_1,
      !!axis_2,
      time
    ) %>%
    group_by(
      !!axis_1,
      !!axis_2,
      time
    ) %>%
    mutate(mass = n()) %>%
    ungroup()

  # Plot der Daten
  if (inherits(res_gda, c("MCA"))) {
    p <- .create_plot()
  } else {
    stop("Only MCA plots are currently supported!")
  }

  # Concentartion ellipse
  if (concentration_ellipse) {

    p <-
      p +
      stat_ellipse(
        data = .count_distinct_ind(res_gda),
        aes(x, y),
        geom ="polygon",
        level = 0.8647,
        type = "norm",
        alpha = 0.1,
        colour = "black",
        linetype = "dotted",
        fill = NA,
        segments = 500
      )

    if (ind_points) {
      p <-
        p +
        geom_point(
          data = .count_distinct_ind(res_gda),
          aes(x, y, size = count),
          colour = "black",
          alpha = 0.1,
          show.legend = FALSE
        )
    }

  }

  # Calculate sample ellipse

  ellipse_axes <- NULL

  for (i in seq_along(time_point_names)) {

    p_calc <-
      ggplot() +
      stat_ellipse(
        data = coord_all %>%
          filter(time == time_point_names[i]),
        aes(
          !!axis_1,
          !!axis_2
        ),
        segments = 500,
        type = "norm",
        level = 0.86,
        show.legend = FALSE
      )

    # Get ellipse coords from plot
    pb <- ggplot_build(p_calc)
    el <- pb$data[[1]][c("x","y")]

    # Calculate centre of ellipse
    ctr <-
      coord_mean %>%
      filter(time == time_point_names[i]) %>%
      select(
        x = paste0("Dim.", axes[1]),
        y = paste0("Dim.", axes[2])
      ) %>%
      as.matrix() %>%
      as.vector()

    # Calculate distance to centre from each ellipse pts
    dist2center <- sqrt(rowSums(t(t(el) - ctr)^2))

    # Identify axes points
    df <-
      bind_cols(
        el,
        dist2center = dist2center,
        time = rep(time_point_names[i], length(dist2center))
      ) %>%
      arrange(dist2center) %>%
      slice(c(1, 2, n() - 1, n())) %>%
      mutate(dist2center = round(dist2center, 2))

    # Store results
    ellipse_axes <- bind_rows(ellipse_axes, df)
  }

  odd_indexes <- seq(1,nrow(ellipse_axes), 2)

  even_indexes <- seq(2,nrow(ellipse_axes), 2)

  start_points <-
    ellipse_axes %>%
    slice(odd_indexes)

  end_points <-
    ellipse_axes %>%
    slice(even_indexes) %>%
    select(xend = x, yend = y, dist2center, time)

  ellipse_axes <- full_join(start_points, end_points, by = c("dist2center", "time"))

  # Plot Ellipse
  p <-
    p +
    stat_ellipse(
      data = coord_all,
      aes(
        !!axis_1,
        !!axis_2,
        fill = time,
        colour = time
      ),
      geom = "polygon",
      type = "norm",
      alpha = alpha,
      segments = 500,
      level = 0.8647,
      linetype = "solid",
      show.legend = FALSE
    )

  p <-
    p +
    geom_segment(
      data = ellipse_axes,
      aes(x = x, xend = xend, y = y, yend = yend, group = dist2center, colour = time),
      linetype = "dashed",
      inherit.aes = FALSE,
      show.legend = FALSE
    )

  if (ind_points) {
    p <-
      p +
      geom_point(
        data = coord_all,
        aes(
          !!axis_1,
          !!axis_2,
          colour = time,
          size = mass
        ),
        show.legend = FALSE)
  }

  p <-
      p +
      geom_point(
          data = coord_mean_mass,
          aes(
              !!axis_1,
              !!axis_2,
              size = mass,
              fill = time
          ),
          colour = "black",
          shape = 23,
          show.legend = TRUE
      ) +
        geom_path(
          data = coord_mean_mass,
          aes(
            !!axis_1,
            !!axis_2
          ),
          size = 1,
          arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
          show.legend = FALSE
        ) +
        scale_size_continuous(guide = "none") +
        scale_colour_viridis_d(aesthetics = c("colour", "fill"))

  # Dimensionen anpassen
  if (!is_null(xlim)) {
      p <-
          p +
          scale_x_continuous(
              limits = xlim,
              breaks = seq(round(xlim[1]), round(xlim[2]), by = 0.5)
          )
  }
  if (!is_null(ylim)) {
      p <-
          p +
          scale_y_continuous(
              limits = ylim,
              breaks = seq(round(ylim[1]), round(ylim[2]), by = 0.5)
          )
  }

  # Plot aufbereiten und finalisieren
  p <- .finalize_plot(
      plot = p,
      res_gda = res_gda,
      axes = axes,
      labels = labels,
      axis_label_y_vjust = 0.99,
      axis_label_x_hjust = 0.99,
      xlim = xlim,
      ylim = ylim
  )

  p <- .annotate_axes(p, labels, alpha = axes_annotate_alpha)

  if (!is_null(title)) p <- p + ggtitle(title)

  # Beschreibung der Punkte
  p <-
    p +
    theme(
      plot.title = element_blank(),
      legend.position = c(legend_x, legend_y),
      legend.box.background = element_rect(
        linetype = "solid",
        colour = "gray17",
        fill = "white"
      ),
      legend.text = element_text(size = 10),
      legend.box.margin = margin(0, 0.2, 0.1, 0, "cm"),
      legend.title = element_blank()
    ) +
    guides(
      colour = guide_legend(
        override.aes = list(size = 4))
    )

  # Plotten
  p

}
