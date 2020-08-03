#' @include utilities.R
#' @include add-theme.R
#' @include get-gda-trajectory.R
NULL
#' Visualization of trajectory structuring factor ellipses.
#'
#' @param res_gda MCA result (rownames have to be questionnaire IDs including time number, e.g. 87654_1).
#' @param df_var_quali data frame containing one qualitative variable (with IDs as rownames).
#' @param var_quali name of the structuring variable.
#' @param axes the axes to plot.
#' @param open_sans use Open Sans font (boolean).
#' @param time_point_names vector containing the name of the time points.
#' @param ind_points show individuals (boolean).
#' @param title title of the plot.
#' @param impute use imputation for missing data.
#' @param concentration_ellipse plot confidence ellipses (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param alpha ellipse fill alpha.
#' @param select choose cluster/category.
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#' @param axes_annotate_alpha alpha value of axes annotations.
#' @param select_facet facet clusters/categories (boolean.)
#' @param xlim x limits.
#' @param ylim y limits.
#'
#' @return ggplot2 visualization.
#' @export
fviz_gda_trajectory_ellipses <- function(res_gda,
                                         df_var_quali,
                                         var_quali,
                                         axes = 1:2,
                                         open_sans = TRUE,
                                         impute = TRUE,
                                         time_point_names = NULL,
                                         ind_points = TRUE,
                                         concentration_ellipse = TRUE,
                                         title = NULL,
                                         plot_modif_rates = TRUE,
                                         alpha = 0.15,
                                         select = NULL,
                                         select_facet = TRUE,
                                         labels = NULL,
                                         xlim = NULL,
                                         ylim = NULL,
                                         axes_annotate_alpha = 0.3) {

  # Add Open Sans font family
  if (open_sans) .add_fonts()

  # Evaluate axes
  axis_1 <- sym(paste0("Dim.", axes[1]))
  axis_2 <- sym(paste0("Dim.", axes[2]))

  # Trajektoriedaten zusammenstellen
  coord_trajectory <- get_gda_trajectory(res_gda, time_point_names)
  coord_all = coord_trajectory$coord_all
  coord_mean_mass = coord_trajectory$coord_mean_mass
  time_point_names <- coord_trajectory$time_point_names

  # Datensatz für zusätzliche Variable konstruieren
  # @TODO replace deprecated _ functions!
  df_quali <-
    df_var_quali %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "id") %>%
    select(id, var_quali = {{ var_quali }})
  df_base <-
    res_gda$call$X %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    separate(rowname, c("id", "time"), sep = "_", fill = "right")
  df_full <-
    full_join(df_base, df_quali, by = "id") %>%
    mutate_all(as.factor) %>%
    select(-id, -time) %>%
    as.data.frame()

  # Imputation
  if (impute) {
    message("Info: Missing data will be imputed!")
    df_full <- imputeMCA(df_full)$completeObs
  }

  # Datensatz um qualitative Variable ergänzen, um Gruppierungen vorzunehmen.
  coord_var_quali <-
    bind_cols(
      coord_all,
      tibble(var_quali = df_full$var_quali)
    ) %>%
    select(
      !!axis_1,
      !!axis_2,
      var_quali,
      time
    )

  if (!is_null(select) & select_facet) {
    coord_var_quali <-
      coord_var_quali %>%
      group_by(var_quali, time) %>%
      mutate(
        count = n()
      ) %>%
      ungroup() %>%
      group_by(time) %>%
      mutate(
        time = fct_inorder(as_factor(str_glue(
        "<b>{time}</b><br>
        <span style='font-size:9pt'>
        {format(round(count/n() * 100, 1), decimal.mark=',')} %, n = {count}
        </span>"
      )))
      ) %>%
      ungroup() %>%
      select(-count)
  }

  coord_var_quali <-
    coord_var_quali %>%
    group_by_all() %>%
    mutate(mass = n()) %>%
    ungroup()

  # Behandlung von fehlenden Werten
  if (!impute) {
    message("Info: Missing data will be excluded!")
    coord_var_quali <- na.omit(coord_var_quali)
  }

  # Mittelwerte und Gruppengewicht berechnen
  coord_mean_var_quali <-
    coord_var_quali %>%
    select(-mass) %>%
    group_by(time, var_quali) %>%
    summarise_all(mean)
  coord_mass_var_quali <-
    coord_var_quali %>%
    count(var_quali, time) %>%
    rename(mass = n)
  coord_mean_mass_var_quali <-
    full_join(coord_mean_var_quali, coord_mass_var_quali, by = c("time", "var_quali"))

  # Plot der Daten
  if (inherits(res_gda, c("MCA"))) {
    p <- .create_plot()
  } else {
    stop("Only MCA plots are currently supported!")
  }

  if (!is_null(xlim)) {
    p <- p + xlim(xlim)
  }

  if (!is_null(ylim)) {
    p <- p + ylim(ylim)
  }

  # Concentartion ellipse
  if (concentration_ellipse) {
    p <-
      p +
      stat_ellipse(
        data = .count_distinct_ind(res_gda),
        aes(x, y),
        geom = "polygon",
        level = 0.8647,
        type = "norm",
        alpha = 0.1,
        colour = "black",
        linetype = "dashed",
        segments = 500,
        fill = NA,
        show.legend = FALSE
      )
  }

  # Quali ellipses
  ellipse_axes <- NULL

  for (i in seq_along(coord_mean_mass_var_quali %>% .$time)) {

    p_calc <-
      ggplot() +
      stat_ellipse(
        data = coord_var_quali %>%
          filter(
            var_quali == coord_mean_mass_var_quali$var_quali[i] &
              time == coord_mean_mass_var_quali$time[i]
          ),
        aes(
          !!axis_1,
          !!axis_2
        ),
        segments = 500,
        type = "norm",
        level = 0.86
      )

    # Get ellipse coords from plot
    pb <- ggplot_build(p_calc)
    el <- pb$data[[1]][c("x","y")]

    # Calculate centre of ellipse
    ctr <-
      coord_mean_mass_var_quali %>%
      ungroup() %>%
      filter(
        var_quali == coord_mean_mass_var_quali$var_quali[i] &
          time == coord_mean_mass_var_quali$time[i]
      ) %>%
      select(
        !!axis_1,
        !!axis_2,
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
        var_quali = rep(
          coord_mean_mass_var_quali$var_quali[i],
          length(dist2center)
        ),
        time = rep(
          coord_mean_mass_var_quali$time[i],
          length(dist2center))
      ) %>%
      arrange(dist2center) %>%
      slice(c(1, 2, n() - 1, n())) %>%
      mutate(dist2center = round(dist2center, 2))

    # Store results
    ellipse_axes <-
      bind_rows(ellipse_axes, df) %>%
      mutate(group = paste(dist2center, var_quali))

  }

  odd_indexes <- seq(1,nrow(ellipse_axes), 2)

  even_indexes <- seq(2,nrow(ellipse_axes), 2)

  start_points <-
    ellipse_axes %>%
    slice(odd_indexes)

  end_points <-
    ellipse_axes %>%
    slice(even_indexes) %>%
    select(xend = x, yend = y, group, time)

  ellipse_axes <- full_join(start_points, end_points, by = c("group", "time"))

  # if( !is.null(relevel) ) ellipse_axes <- ellipse_axes %>% mutate(var_quali = fct_relevel(var_quali, relevel))

  # Filter data
  if (!is_null(select)) {

    coord_var_quali <-
      coord_var_quali %>%
      filter(var_quali %in% select)

    ellipse_axes <-
      ellipse_axes %>%
      filter(var_quali %in% select)

    coord_mean_mass_var_quali <-
      coord_mean_mass_var_quali %>%
      filter(var_quali %in% select)

    p <-
      p +
      stat_ellipse(
        data = coord_var_quali,
        aes(
          !!axis_1,
          !!axis_2,
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
  } else {

    p <-
      p +
      stat_ellipse(
        data = coord_var_quali,
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

  }

  p <-
    p +
    geom_segment(
      data = ellipse_axes,
      aes(x = x, xend = xend, y = y, yend = yend, group = group, colour = time),
      linetype = "dashed",
      inherit.aes = FALSE,
      show.legend = FALSE
    )

  if (ind_points) {
    p <-
      p +
      geom_point(
        data = coord_var_quali,
        aes(
          !!axis_1,
          !!axis_2,
          colour = time,
          size = mass
        ),
        show.legend = FALSE
      )
  }

  p <-
    p +
    geom_point(
      data = coord_mean_mass_var_quali,
      aes(
        !!axis_1,
        !!axis_2,
        size = mass * 1.75
      ),
      colour = "black",
      shape = 18,
      show.legend = FALSE
    ) +
    geom_point(
      data = coord_mean_mass_var_quali,
      aes(
        !!axis_1,
        !!axis_2,
        size = mass,
        colour = time
      ),
      shape = 18,
      show.legend = TRUE
    ) +
    scale_size_continuous(guide = FALSE) +
    guides(
      colour = guide_legend(
        override.aes = list(size = 4))
    )

  if (is_null(select) | (length(select) == 1 & !select_facet)) {
    p <-
      p +
      geom_path(
        data = coord_mean_mass_var_quali,
        aes(
          !!axis_1,
          !!axis_2
        ),
        size = 1,
        arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
        show.legend = FALSE
      )
  }

  # Beschriftung anpassen
  p <- .finalize_plot(
    p,
    res_gda,
    axes,
    labels,
    axis_label_y_vjust = 0.99,
    axis_label_x_hjust = 0.99
  )

  p <- .annotate_axes(p, labels, alpha = axes_annotate_alpha)

  # Beschreibung der Punkte
  if (length(select) > 1 | is_null(select)) {
    p <-
      p +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )
  }

  if (select_facet & length(select) == 1) {
    p <-
      p +
      facet_wrap(~time) +
      theme(
        panel.border = element_rect(
          size = 1,
          fill = NA,
          colour = "gray17"
        ),
        plot.margin = margin(0.5, 0, 0, 0, "cm"),
        panel.spacing = unit(0.5, "cm"),
        strip.text = element_textbox(
          halign = 0.5,
          size = 12,
          margin = unit(c(0.5, 0, 0, 0), "mm")
        )
      )
  }

  if (is_null(select)) {
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
          family = "Fira Sans Condensed",
          vjust = 0.5,
          hjust = 0.5,
          size = 12,
          margin = margin(0, 0, 3, 0, "mm")
        )
      )

  }

  if (!is_null(title)) p <- p + ggtitle(title)

    # Plotten
   p

}
