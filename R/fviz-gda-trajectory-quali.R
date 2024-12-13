#' @include utilities.R
#' @include add-theme.R
#' @include get-gda-trajectory.R
NULL
#' Visualize trajectories and structuring factors.
#'
#' @param res_gda MCA result (rownames have to be questionnaire IDs including time number, e.g. 87654_1).
#' @param df_var_quali data frame containing one qualitative variable (with IDs as rownames).
#' @param var_quali name of the structuring variable.
#' @param axes the axes to plot.
#' @param ind_labels plot labels (boolean).
#' @param select select vector of names, within_inertia of individuals selection (within_inertia: vector containing the number of high variation and low variationindividuals) or case (vector containing NULL, complete, or incomplete).
#' @param title the plot title.
#' @param time_point_names vector containing the name of the time points.
#' @param impute use imputation for missing data.
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#' @param axes_annotate_alpha alpha value of axes annotations.
#' @param xlim numeric vector of 2.
#' @param ylim numeric vector of 2.
#' @param var_quali_select the name of the selected categories/clusters.
#' @param case_names named character vector containing names of cases.
#' @param label_x_limits constrain the labels to a specific area. Limits are specified in data coordinates.
#' @param label_y_limits constrain the labels to a specific area. Limits are specified in data coordinates.
#'
#' @return ggplot2 visualization.
#' @export
fviz_gda_trajectory_quali <- function(res_gda, df_var_quali, var_quali, var_quali_select = NULL, axes = 1:2,
                                      ind_labels = FALSE, title = NULL, time_point_names = NULL,
                                      select = list(name = NULL, within_inertia = NULL, case = NULL), impute = TRUE,
                                      plot_modif_rates = TRUE, labels = NULL, xlim = NULL, ylim = NULL,
                                      axes_annotate_alpha = 0.3, case_names = NULL, label_x_limits = NA,
                                      label_y_limits = NA) {

  # Evaluate axes
  axis_1 <- sym(paste0("Dim.", axes[1]))
  axis_2 <- sym(paste0("Dim.", axes[2]))

  # Trajektoriedaten zusammenstellen
  coord_trajectory <- get_gda_trajectory(res_gda, time_point_names)
  coord_all = coord_trajectory$coord_all
  time_point_names <- coord_trajectory$time_point_names

  # Datensatz für zusätzliche Variable konstruieren
  df_quali <-
    df_var_quali %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "id") %>%
    select(id, var_quali = {{ var_quali }})
  df_base <-
    res_gda$call$X %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    separate(rowname, c("id", "time"), sep = "_", fill = "right")
  df_full <-
    full_join(df_base, df_quali, by = "id") %>%
    mutate_all(as.factor) %>%
    select(-id, -time) %>%
    data.frame()

  # Imputation
  if (impute) {
    message("Info: Missing data will be imputed!")
    df_full <- imputeMCA(df_full)$completeObs
  }

  # Datensatz um qualitative Variable ergänzen, um Gruppierungen vorzunehmen.
  coord_var_quali <- bind_cols(coord_all, tibble(var_quali = df_full$var_quali))

  # Filtern nach Cluster
  if (!is_null(var_quali_select)) {
    df_full <-
      df_full %>%
      filter(var_quali %in% var_quali_select)

    coord_var_quali <-
      coord_var_quali %>%
      filter(var_quali %in% var_quali_select)

    coord_all <- coord_all %>% filter(id %in% coord_var_quali$id)
  }

  # Behandlung von fehlenden Werten
  if (!impute) {
    message("Info: Missing data will be excluded!")
    coord_var_quali <- na.omit(coord_var_quali)
    # Nur vorhandene Fälle verwenden
    coord_all <- coord_all %>% filter(id %in% coord_var_quali$id)
  }

  # Auswahl vornehmen
  selected_ind <- .select_trajectory(coord_all, select, time_point_names, axes)

  # Filterung vornehmen
  coord_ind_timeseries <-
      coord_var_quali %>%
      filter(id %in% selected_ind$id) %>%
      mutate(name = id)

  # Namen hinzufügen
  if (!is_null(case_names)) {
    coord_ind_timeseries <-
        coord_ind_timeseries %>%
        mutate(
            name = fct_recode(
                as_factor(name),
                !!!case_names
            )
        )
  }

  # Ausgabe der standardisierten Distanzen
  ind_dist <- function(dim, res_gda, coord) {
      dist_make(
          coord[dim] %>% as.matrix(),
          function (v1, v2) abs((v1 - v2)/sqrt(res_gda$eig$eigenvalue[dim]))
      )
  }

  dim_n <- res_gda$call$ncp

  dims <- setNames(1:dim_n, paste0("Dim.", 1:dim_n))

  dist <-
      dims %>%
      map(~ ind_dist(
          .,
          res_gda,
          coord_ind_timeseries %>%
            select(-id, -var_quali) %>%
            mutate(name = paste(name, time)) %>%
            select(-time) %>%
            arrange(name) %>%
            column_to_rownames(var = "name")
        )
    )

  message("Standardized Distances:")
  print(dist[axes])

  # Plot der Daten
  if (inherits(res_gda, c("MCA"))) {
    p <- .create_plot()
  } else {
    stop("Only MCA plots are currently supported!")
  }

  p <-
    p +
    # scale_colour_brewer(palette = "YlGnBu", direction = -1) +
    scale_colour_viridis_d() +
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
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    )

  # Labeln
  if (ind_labels) {
    p <-
      p +
      ggrepel::geom_label_repel(
        data = coord_ind_timeseries %>%
            # Alle drei Punkte pro Person, aber nur ersten Punkt mit Label für repel Algorithmus.
            mutate(
                point_label = if_else(
                    time == time_point_names[1],
                    as.character(name),
                    "")
        ),
        show.legend = FALSE,
        aes(
          !!axis_1,
          !!axis_2,
          colour = time,
          label = point_label
        ),
        point.padding = 0,
        min.segment.length = 0,
        box.padding = 0.5,
        xlim  = label_x_limits,
        ylim  = label_y_limits,
        max.overlaps = 20
      )
  }

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

  # Beschriftung anpassen
  p <- .finalize_plot(
    p,
    res_gda,
    axes,
    labels,
    axis_label_y_vjust = 0.99,
    axis_label_x_hjust = 0.99,
    xlim = xlim,
    ylim = ylim
  )

  p <- .annotate_axes(p, labels, alpha = axes_annotate_alpha)

  # Aufteilen
  p <-
    p +
    facet_wrap(~var_quali) +
    theme(
      panel.border = element_rect(
        size = 1,
        fill = NA,
        colour = "gray17"
      ),
      panel.spacing = unit(0.5, "cm"),
      strip.text.x = element_text(
        face = "bold",
        family = "Fira Sans Condensed Medium",
        vjust = 0.5,
        hjust = 0.5,
        size = 14,
        margin = unit(c(0.5, 0, 1, 0), "mm")

      ),
      legend.position = "bottom",
      legend.title = element_blank()
    )

  if (!is_null(title)) p <- p + ggtitle(title)

  # Plotten
  p

}
