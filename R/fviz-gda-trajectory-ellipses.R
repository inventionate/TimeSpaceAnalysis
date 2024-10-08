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
#' @param time_point_names vector containing the name of the time points.
#' @param ind_points show individuals (boolean).
#' @param title title of the plot.
#' @param impute use imputation for missing data.
#' @param concentration_ellipse plot concentration ellipses (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param alpha ellipse fill alpha.
#' @param select choose cluster/category.
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#' @param axes_annotate_alpha alpha value of axes annotations.
#' @param select_facet facet clusters/categories (boolean.)
#' @param xlim x limits.
#' @param ylim y limits.
#' @param complete_obs plot only complete observations (boolean).
#' @param facet_title_size size of the facet stripe title (numeric).
#' @param density should 2D density lines be drawn (boolean).
#' @param ellipses should ellipses be drawn (boolean).
#'
#' @return ggplot2 visualization.
#' @export
fviz_gda_trajectory_ellipses <- function(res_gda, df_var_quali, var_quali, axes = 1:2, impute = TRUE,
                                         time_point_names = NULL, ind_points = TRUE, concentration_ellipse = TRUE,
                                         title = NULL, plot_modif_rates = TRUE, alpha = 0.15, select = NULL,
                                         select_facet = TRUE, labels = NULL, xlim = NULL, ylim = NULL,
                                         axes_annotate_alpha = 0.3, complete_obs = FALSE,
                                         facet_title_size = 14, density = FALSE, ellipses = TRUE) {

  # Evaluate axes
  axis_1 <- sym(paste0("Dim.", axes[1]))
  axis_2 <- sym(paste0("Dim.", axes[2]))

  # Trajektoriedaten zusammenstellen
  coord_trajectory <- get_gda_trajectory(res_gda, time_point_names, complete_obs)
  coord_all <- coord_trajectory$coord_all
  coord_all_complete <- coord_trajectory$coord_all_complete
  time_point_names <- coord_trajectory$time_point_names

  # Datensatz für zusätzliche Variable konstruieren
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
  df_full_id <-
    full_join(df_base, df_quali, by = "id") %>%
    mutate_all(as_factor)

  if (complete_obs) {
    df_full_complete <-
      df_full_id %>%
      filter(id %in% coord_all_complete$id) %>%
      select(-id, -time) %>%
      as.data.frame()

    message("Info: Complete cases filtered!")
  }

  df_full <-
    df_full_id %>%
    select(-id, -time) %>%
    as.data.frame()

  # Imputation
  if (impute) {
    message("Info: Missing data will be imputed!")
    df_full <- imputeMCA(df_full)$completeObs

    if (complete_obs) {
      df_full_complete <- imputeMCA(df_full_complete)$completeObs
    }
  }

  # Datensatz um qualitative Variable ergänzen, um Gruppierungen vorzunehmen.
  # @TODO: Code ist zu kompliziert und repetitiv! Refactor!
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
    ) %>%
    group_by(var_quali, time) %>%
    mutate(
      count = n()
    ) %>%
    ungroup() %>%
    mutate(
      time_short = time
    )

  if (complete_obs) {
    coord_var_quali_complete <-
      bind_cols(
        coord_all_complete,
        tibble(var_quali = df_full_complete$var_quali)
      ) %>%
      select(
        !!axis_1,
        !!axis_2,
        var_quali,
        time
      )
  }

  if (!is_null(select) & select_facet) {
    coord_var_quali <-
      coord_var_quali %>%
      group_by(var_quali, time) %>%
      mutate(
        count = n()
      ) %>%
      ungroup() %>%
      mutate(time_short = time) %>%
      group_by(time) %>%
      mutate(
        time = fct_inorder(as_factor(str_glue(
          "<b>{time}</b><br>
        <span style='font-size:{facet_title_size - 3}pt'>
        {format(round(count/n() * 100, 1), decimal.mark=',')} %, n = {count}
        </span>"
        )))
      ) %>%
      ungroup()

    count_selected_overall <-
      coord_var_quali %>%
      filter(var_quali %in% select) %>%
      slice(1) %>%
      pull(count)

    if (!complete_obs) {
      coord_var_quali <-
        coord_var_quali %>%
        select(-count, -time_short)
    }
  }

  if (complete_obs) {
    if (is_null(select)) {
      select <- df_full %>%
        pull(var_quali) %>%
        as_factor() %>%
        levels()
    }

    coord_var_quali_all <-
      coord_var_quali %>%
      select(-count) %>%
      group_by_all() %>%
      mutate(mass = n()) %>%
      ungroup() %>%
      filter(time_short == time_point_names[[1]]) %>%
      select(-time, -time_short) %>%
      filter(var_quali %in% select)

    coord_mean_var_quali_all <-
      coord_var_quali_all %>%
      select(-mass) %>%
      group_by(var_quali) %>%
      summarise_all(mean)

    coord_mass_var_quali_all <-
      coord_var_quali_all %>%
      count(var_quali) %>%
      rename(mass = n)

    coord_mean_mass_var_quali_all <-
      full_join(
        coord_mean_var_quali_all,
        coord_mass_var_quali_all,
        by = "var_quali"
      )
  }

  # Die Anzahlen berechnen
  if (complete_obs & !select_facet) {
    count_selected_complete <-
      coord_var_quali_complete %>%
      filter(var_quali %in% select) %>%
      filter(time == time_point_names[[1]]) %>%
      nrow()

    count_selected_overall <-
      coord_var_quali_all %>%
      nrow()
  }

  if (!is_null(select) & select_facet & complete_obs) {
    coord_var_quali <-
      coord_var_quali_complete %>%
      group_by(var_quali, time) %>%
      mutate(
        count = n()
      ) %>%
      ungroup() %>%
      mutate(time_short = time) %>%
      group_by(time) %>%
      mutate(
        time = fct_inorder(as_factor(str_glue(
          "<b>{time}</b>"
        )))
      ) %>%
      ungroup()

    count_selected_complete <-
      coord_var_quali %>%
      filter(var_quali %in% select) %>%
      slice(1) %>%
      pull(count)

    coord_var_quali <-
      coord_var_quali %>%
      select(-count)
  }

  if (complete_obs) {
    coord_var_quali <-
      coord_var_quali_complete %>%
      mutate(
        time_short = time
      )
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
    select(-mass, -time_short) %>%
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
        linetype = "dotted",
        segments = 500,
        fill = NA,
        show.legend = FALSE
      )
  }

  if (complete_obs) {

    # Calculate ellipsis axes
    p_calc <-
      ggplot() +
      stat_ellipse(
        data = coord_var_quali_all,
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
    el <- pb$data[[1]][c("x", "y")]

    # Calculate centre of ellipse
    ctr <-
      coord_mean_mass_var_quali_all %>%
      ungroup() %>%
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
        dist2center = dist2center
      ) %>%
      arrange(dist2center) %>%
      slice(c(1, 2, n() - 1, n())) %>%
      mutate(dist2center = round(dist2center, 2))

    # Store results
    odd_indexes <- seq(1, nrow(df), 2)

    even_indexes <- seq(2, nrow(df), 2)

    start_points <-
      df %>%
      slice(odd_indexes)

    end_points <-
      df %>%
      slice(even_indexes) %>%
      rename(xend = x, yend = y)

    ellipse_axes_all <- full_join(start_points, end_points, by = "dist2center")

    # Standardisierte Distanzen hinzufügen
    gda_dist <- function(dim, res_gda, coord) {
        dist_make(
            coord[dim] %>% as.matrix(),
            function (v1, v2) abs((v1 - v2)/sqrt(res_gda$eig$eigenvalue[dim]))
        )
    }

    coord <-
        coord_mean_var_quali %>%
        ungroup() %>%
        filter(var_quali == select) %>%
        select(-time, -var_quali)

    dim_n <- length(axes)

    dims <- setNames(1:dim_n, paste0("Dim.", 1:dim_n))

    dist <-
        dims %>%
        map(~ gda_dist(., res_gda, coord))

    message("Distanzen der Schwerpunkte (SD):")
    print(dist)

    if (ellipses) {
        p <-
          p +
          stat_ellipse(
            data = coord_var_quali_all,
            aes(
              !!axis_1,
              !!axis_2
            ),
            colour = "gray80",
            fill = NA,
            geom = "polygon",
            type = "norm",
            alpha = 1,
            segments = 500,
            level = 0.8647,
            linetype = "solid",
            show.legend = FALSE
          ) +
          geom_segment(
            data = ellipse_axes_all,
            aes(x = x, xend = xend, y = y, yend = yend),
            colour = "gray80",
            linetype = "dashed",
            inherit.aes = FALSE,
            show.legend = FALSE
          )
    }

    if (ind_points) {
      p <-
        p +
        geom_point(
          data = coord_var_quali_all,
          aes(
            !!axis_1,
            !!axis_2
          ),
          colour = "gray80",
          show.legend = FALSE
        )
    }

    if (density) {
      p <-
        p +
        geom_density_2d(
          data = coord_var_quali_all,
          aes(
            !!axis_1,
            !!axis_2
          ),
          colour = "gray80",
          show.legend = FALSE
        )
    }

    p <-
      p +
      geom_point(
        data = coord_mean_mass_var_quali_all,
        aes(
          !!axis_1,
          !!axis_2,
          size = mass,
          fill = time
        ),
        fill = "gray60",
        colour = "black",
        shape = 23,
        show.legend = TRUE
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
    el <- pb$data[[1]][c("x", "y")]

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
          length(dist2center)
        )
      ) %>%
      arrange(dist2center) %>%
      slice(c(1, 2, n() - 1, n())) %>%
      mutate(dist2center = round(dist2center, 2))

    # Store results
    ellipse_axes <-
      bind_rows(ellipse_axes, df) %>%
      mutate(group = paste(dist2center, var_quali))
  }

  odd_indexes <- seq(1, nrow(ellipse_axes), 2)

  even_indexes <- seq(2, nrow(ellipse_axes), 2)

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

    if (ellipses) {
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
        ) +
          geom_segment(
              data = ellipse_axes,
              aes(x = x, xend = xend, y = y, yend = yend, group = group, colour = time),
              linetype = "dashed",
              inherit.aes = FALSE,
              show.legend = FALSE
          )
    }
  } else {
    if (ellipses) {
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
        ) +
      geom_segment(
        data = ellipse_axes,
        aes(x = x, xend = xend, y = y, yend = yend, group = group, colour = time),
        linetype = "dashed",
        inherit.aes = FALSE,
        show.legend = FALSE
      )
    }
  }

  if (ind_points) {
    p <-
      p +
      geom_point(
        data = coord_var_quali,
        aes(
          !!axis_1,
          !!axis_2,
          colour = time
        ),
        show.legend = FALSE
      )
  }

  if (density) {
    p <-
      p +
      geom_density_2d(
        data = coord_var_quali,
        aes(
          !!axis_1,
          !!axis_2,
          colour = time
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
        size = mass,
        fill = time
      ),
      colour = "black",
      shape = 23,
      show.legend = TRUE
    ) +
    scale_size_continuous(guide = "none", range = c(3, 5)) +
    guides(
      colour = guide_legend(
        override.aes = list(size = 4)
      )
    ) +
    scale_colour_viridis_d(aesthetics = c("colour", "fill"))

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

  # Beschreibung der Punkte
  # if (length(select) > 1 | is_null(select)) {
  p <-
    p +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )


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
          size = facet_title_size,
          margin = unit(c(0.5, 0, 1, 0), "mm"),
          family = "Fira Sans Condensed Medium",
          face = "bold"
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
          family = "Fira Sans Condensed Medium",
          vjust = 0.5,
          hjust = 0.5,
          size = 14,
          margin = margin(0.1, 0, 1, 0, "mm")
        )
      )
  }

  if (!is_null(title)) {
    p <-
      p +
      ggtitle(title)
  }

  if (complete_obs) {
    p <-
      p +
      labs(
        caption = str_glue("{count_selected_complete} vollständige Fälle von insgesamt {count_selected_overall} dieser Konstellation.")
      ) +
      theme(
        plot.caption = element_text(size = 10)
      )
  }

  # Plotten
  p
}
