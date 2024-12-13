#' @include utilities.R
NULL
#' Title
#'
#' @param res_gda GDA (MCA, MFA) result (rownames have to be individual questionnaire IDs).
#' @param df_var_quali data frame of one quali variable.
#' @param var_quali name if quali variable.
#' @param title plot title.
#' @param facet whether facet ellipses or not (boolean).
#' @param scale_mean_points scale mean point size in respect of the group size (boolean).
#' @param axes the GDA dimensions to plot.
#' @param colour Colour brewer scale or FALSE.
#' @param alpha_point opacity of individual points.
#' @param conc_linetype linetype of concentration ellipses.
#' @param conf_linetype linetype of confidence ellipses.
#' @param concentration_ellipses plot concentration ellipse (boolean).
#' @param confidence_ellipses plot confidence ellipses (boolean).
#' @param conf_colour colour confidence ellipses (boolean).
#' @param impute impute missing data (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param ncol Number of facet columns.
#' @param individuals show individual points (boolean).
#' @param impute_ncp number of dimensions to predict missing values.
#' @param reorder numeric vector containing new level order (index).
#' @param alpha_ellipses concentration ellipses fill alpha.
#' @param print_eta2 print eta2 value per axis (boolean).
#' @param axis_lab_name name of axis label.
#' @param label_mean_points show labels (boolean).
#' @param highlight show facets with highlighted group (boolean).
#' @param profiles optional add specific profiles (tibble).
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#' @param axes_annotate_alpha alpha value of axes annotations.
#' @param density show density contours (boolean).
#' @param global_conc_ellipses should the global concentration ellipse be shown (boolean).
#' @param in_freq order by number of observations with each level (largest first) (boolean).
#' @param facet_title_size size of the facet stripe title (numeric).
#'
#' @return ggplot2 visualization with concentration and quali var ellipses.
#' @export
fviz_gda_quali_ellipses <- function(res_gda, df_var_quali, var_quali, title = NULL, facet = TRUE, alpha_point = 0.75,
                                    conc_linetype = "solid", conf_linetype = "solid", scale_mean_points = TRUE,
                                    axes = 1:2, colour = "Set1", impute = TRUE, concentration_ellipses = TRUE,
                                    confidence_ellipses = FALSE, conf_colour = FALSE, plot_modif_rates = TRUE, ncol = 3,
                                    individuals = TRUE, impute_ncp = 2, reorder = NULL, alpha_ellipses = 0.15,
                                    print_eta2 = TRUE, axis_lab_name = "Achse", label_mean_points = TRUE,
                                    highlight = FALSE, profiles = NULL, labels = NULL, axes_annotate_alpha = 0.3,
                                    density = FALSE, global_conc_ellipses = TRUE, in_freq = FALSE,
                                    facet_title_size = 14) {

# ---------------------------------------------------------------------------------------------


  # Datensatz auslesen
  var <-
    df_var_quali %>%
    select(var_quali = {{ var_quali }}) %>%
    mutate_all(as.character)

  #eta2 extrahieren
  if (print_eta2) {

    # Berechnungen der passiven Variable durchführen
    supvar_stats <-
      supvar_stats(
        res_gda,
        df_var_quali,
        {{ var_quali }},
        impute,
        impute_ncp
      )

    supvar_eta2 <-
      bind_cols(
        rowname = rownames(supvar_stats$var),
        supvar_stats$var
      ) %>%
      filter(rowname == "eta2") %>%
      select(-rowname) %>%
      mutate_all(~ round(., 3)) %>%
        as_tibble() %>%
        pivot_longer(
            cols = everything(),
            names_to = "Dim",
            values_to = "eta2"
        ) %>%
        mutate(
            "explanation in %" = eta2 * 100
        )

    print(supvar_eta2)
  }

  # Auf Fehlende Werte prüfen.
  exclude_na <- which(is.na(var))

  if (length(exclude_na) != 0) {

    if (impute) {
      message("Info: Missing data will be imputed!")

      var <- var %>% mutate_all(as.factor)

      # Nur aktive Individuen verwenden
      if (!is.null(res_gda$call$ind.sup)) {
        X <- res_gda$call$X[-res_gda$call$ind.sup,]
      } else {
        X <- res_gda$call$X
      }

      if (inherits(res_gda, c("MCA"))) {

        var_impute <- missMDA::imputeMCA(data.frame(X, var), ncp = impute_ncp)

      }

      if (inherits(res_gda, c("MFA"))) {

        var_impute <-
          missMDA::imputeMFA(
            data.frame(X, var),
            c(res_gda$call$group, 1),
            res_gda$call$ncp,
            c(res_gda$call$type, "n"),
            ncp = impute_ncp
          )

      }

      var <- var_impute$completeObs %>%
        as_tibble() %>%
        select(var_quali)
    } else {
      # Fehlende Werte durch Kategorie ersetzen (falls nicht imputiert wurde).
      var[is.na(var)] <- "Fehlender Wert"
    }

  }

  # Beschriftung anpassen
  if (facet) {
    var <-
      var %>%
      group_by(var_quali) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      mutate(var_label = str_glue(
        "<b>{var_quali}</b><br>
      <span style='font-size:{facet_title_size - 3}pt'>
      {format(round(count/n() * 100, 1), decimal.mark=',')} %, n = {count}
      </span>"
      )
      )

    if (!is_null(profiles)) {
        profiles <-
            left_join(profiles, var %>% distinct(), by = "var_quali") %>%
            mutate(var_quali = var_label) %>%
            select(-var_label)
    }

    var <-
        var %>%
        mutate(var_quali = var_label) %>%
        select(-var_label)

  }

  var_levels <-
    var %>%
    select(var_quali) %>%
    mutate_all(as.factor) %>%
    pull(var_quali)

  # Reihenfolge der Levels nach der Anzahl der Fälle
  if (in_freq) var_levels <- fct_infreq(var_levels)

  var_levels <- var_levels %>% levels()

  # Reihenfolge der Levels festlegen
  if (!is.null(reorder)) var_levels <- var_levels[reorder]

  # Spalte in Vektor umwandeln
  var_n <- var %>% count(var_quali, name = "n")
  var <- var %>% pull(var_quali)

  # Datensatz zusammenstellen (Koordinaten mit passiver Variable zusammenführen)
  df_source <-
    tibble(
      x = res_gda$ind$coord[, axes[1]],
      y = res_gda$ind$coord[, axes[2]],
      var_quali = factor(var)
    ) %>%
    mutate(var_quali = factor(var_quali, levels = var_levels))

  coord_ind_quali <- df_source %>%
    group_by(x, y, var_quali) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    mutate(
      var_quali = fct_drop(var_quali),
      colour = var_quali
    )

  coord_mean_quali <-
    coord_ind_quali %>%
    select(-colour) %>%
    group_by(var_quali) %>%
    summarise_all(mean)

  size_mean_quali <-
    coord_ind_quali %>%
    select(-colour) %>%
    group_by(var_quali) %>%
    summarise_all(length) %>%
    ungroup() %>%
    mutate(size = count) %>%
    select(size) %>%
    data.frame()

  coord_mean_quali <-
    bind_cols(coord_mean_quali, size = size_mean_quali) %>%
    mutate(
      prop = str_glue("{var_quali}"),
      prop_desc = str_glue("{format(round(size/sum(size) * 100, 1), decimal.mark=',')} %, n = {size}"),
      colour = var_quali
    )

  # Plot
  if (inherits(res_gda, c("MCA"))) {
    p <- .create_plot()
  } else {
    stop("Only MCA plots are currently supported!")
  }

  # Allgemeine Konzentrationsellipse hinzufügen (level = 86,47% nach Le Roux/Rouanet 2010: 69, da es sich um eine 2-dimesnionale Konzentrationsellipse handelt)
  if (global_conc_ellipses) {
  p <-
    p +
    stat_ellipse(
      data = .count_distinct_ind(res_gda),
      aes(x = x, y = y),
      geom = "polygon",
      level = 0.8647,
      type = "norm",
      alpha = 0.1,
      colour = "black",
      linetype = "dotted",
      segments = 500,
      fill = NA
    )
  }

  # 2D Density contours
  if (density) {
      p <-
          p +
          geom_density_2d(
              data = coord_ind_quali,
              inherit.aes = FALSE,
              aes(x = x,
                  y = y,
                  colour = colour
              ),
              alpha = 0.5
          )
  }

  # Konzentrationsellipsen für die passiven Variablengruppen
  if (individuals) {
    p <-
      p +
      geom_point(
        data = coord_ind_quali,
        aes(x = x,
            y = y,
            colour = colour,
            size = count
        ),
        inherit.aes = FALSE,
        alpha = alpha_point
      )
  }

  p <- p + scale_size_continuous(range = c(1, 7))

    ellipse_axes <- NULL

    for (i in seq_along(var_levels)) {

      p_calc <-
        ggplot() +
        stat_ellipse(
          data = coord_ind_quali %>%
            filter(var_quali == var_levels[i]),
          aes(x, y),
          segments = 500,
          type = "norm",
          level = 0.86
        )

      # Get ellipse coords from plot
      pb <- ggplot_build(p_calc)
      el <- pb$data[[1]][c("x","y")]

      # Calculate centre of ellipse
      ctr <-
        coord_mean_quali %>%
        filter(var_quali == var_levels[i]) %>%
        select(x, y) %>%
        as.matrix() %>%
        as.vector()

      # Calculate distance to centre from each ellipse pts
      dist2center <- sqrt(rowSums(t(t(el) - ctr)^2))

      # Identify axes points
      df <-
        bind_cols(
          el,
          dist2center = dist2center,
          var_quali = rep(var_levels[i], length(dist2center))
        ) %>%
        arrange(dist2center) %>%
        slice(c(1, 2, n() - 1, n())) %>%
        mutate(dist2center = round(dist2center, 2))

      # Store results
      ellipse_axes <-
        bind_rows(ellipse_axes, df) %>%
        mutate(group = paste(dist2center, var_quali))

    }

    ellipse_axes <-
      ellipse_axes %>%
      mutate(
        var_quali = factor(
          var_quali,
          levels = var_levels
        ),
        colour = factor(
            var_quali,
            levels = var_levels
         )
      )

    if (concentration_ellipses) {


    if (colour == FALSE) alpha_ellipses <- 0.01

    p <-
      p +
      stat_ellipse(
        data = coord_ind_quali,
        aes(x = x, y = y, fill = colour, colour = colour),
        geom = "polygon",
        type = "norm",
        alpha = alpha_ellipses,
        linetype = conc_linetype,
        segments = 500,
        level = 0.8647,
        inherit.aes = FALSE
      ) +
      geom_path(
        data = ellipse_axes,
        aes(x = x, y = y, group = group, colour = colour),
        linetype = "dashed",
        inherit.aes = FALSE
      )

    # Der eccentricity coefficient @LeRoux2004: 441 lässt sich über
    # die Distanzangaben zum Mittelpunkt berechnen.
    # Er bewegt sich zwischen 0 (Kreis) und 1 ( Linie) und gibt an,
    # wie stark die Ellipse an einer Achse orintiert ist (diese polarisiert).
  }

  if (confidence_ellipses) {
    conf_ellipses_coord <-
      FactoMineR::coord.ellipse(
        data.frame(coord_ind_quali[c(3,1,2)]), bary = TRUE
      )$res

    if (conf_colour) {
      p <-
        p +
        geom_path(
          data = conf_ellipses_coord,
          aes(x, y, colour = colour),
          show.legend = FALSE,
          linetype = conf_linetype,
          size = 0.75
        )
    } else {
      p <-
        p +
        geom_path(
          data = conf_ellipses_coord,
          aes(x, y, group = colour),
          show.legend = FALSE,
          linetype = conf_linetype,
          size = 0.75
        )
    }
  }

  if (!facet & label_mean_points) {

    df_labels <-
      left_join(ellipse_axes, coord_mean_quali, by = "colour") %>%
      select(
        x = x.x,
        y = y.x,
        colour,
        prop,
        prop_desc
      ) %>%
      as_tibble()

    p <-
      p +
      ggforce::geom_mark_ellipse(
        data = df_labels,
        aes(
          x,
          y,
          group = colour,
          label = prop,
          description = prop_desc
        ),
        color = NA,
        expand = unit(-1, "mm"),
        radius = unit(5, "mm"),
        label.family = "Fira Sans Condensed Medium",
        label.fontsize = c(12, 10),
        label.buffer = unit(5, "mm"),
        label.fill = "gray90",
        # Small fix for label margin
        label.margin = margin(2, 2, 2, 3, "mm"),
        con.size = 0.5
      )
  }

  # Schwerpunkte
  if (scale_mean_points) {
      mapping_point <-
          aes(
              x = x,
              y = y,
              fill = colour,
              size = size
          )

      if (colour == FALSE) mapping_point$colour <- NULL

      if (!scale_mean_points) mapping_point$size <- NULL

      geom_mean_point <-
          geom_point(
              data = coord_mean_quali,
              mapping = mapping_point,
              colour = "#333333",
              shape = 23,
              inherit.aes = FALSE,
              show.legend = FALSE
          )

      if (colour == FALSE) geom_mean_point$aes_params$fill <- "gray75"

      if (!scale_mean_points) geom_mean_point$aes_params$size <- 10

      p <- p + geom_mean_point
  }

  if (colour != FALSE) {
    p <-
      p +
      scale_colour_brewer(
          palette = colour,
          aesthetics = c("colour", "fill")
      )
    } else {
        p <-
            p +
            scale_colour_grey(
                start = 0.2,
                end = 0.2,
                aesthetics = c("colour", "fill")
            )
    }

  if (facet) {

    if (highlight) {
      p <-
        p +
        gghighlight(use_direct_label = FALSE)
    }

    p <- .finalize_plot(
      p,
      res_gda,
      axes,
      labels,
      axis_label_y_vjust = 0.99,
      axis_label_x_hjust = 0.99,
      facet_labels = TRUE
    )

    p <- .annotate_axes(p, labels)

    p <-
      p +
      facet_wrap(
        ~var_quali,
        ncol = ncol
      ) +
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
          margin = unit(c(0.5, 0, 1, 0), "mm")
        )
      )

    # i <- 1
    # j <- 1
    # k <- 1
    # titles <- list()
    # while (i < (nrow(coord_mean_quali) * 2)) {
    #   titles[[i]] <-
    #     draw_label(
    #       coord_mean_quali$prop[[j]],
    #       k/(length(coord_mean_quali$prop) * 2),
    #       facet_label_y + facet_label_sep,
    #       fontfamily = "Fira Sans Condensed Medium",
    #       fontface = "bold")
    #
    #   i <- i + 1
    #
    #   titles[[i]] <- draw_label(
    #       coord_mean_quali$prop_desc[[j]],
    #       k/(length(coord_mean_quali$prop) * 2),
    #       facet_label_y,
    #       fontfamily = "Fira Sans Condensed Medium",
    #       size = 10)
    #
    #     i <- i + 1
    #     j <- j + 1
    #     k <- k + 2
    # }

    p <- p + coord_fixed(1, clip = "off")

  } else {

    p <- .finalize_plot(p, res_gda, axes, labels)

    p <- .annotate_axes(p, labels, alpha = axes_annotate_alpha)

  }

  if (!is_null(profiles)) {

      if (facet) {
          profiles <-
              profiles %>%
              mutate(
                  colour = var_quali
              )
      } else {
          profiles <-
              profiles %>%
              mutate(
                  colour = {{ var_quali }}
              )
      }

      p <-
          p +
          geom_label(
              data = profiles,
              inherit.aes = FALSE,
              aes(x = Dim.1, y = Dim.2, label = name, colour = colour),
              family = "Fira Sans Condensed Medium",
              size = 5,
              alpha = 1,
              show.legend = FALSE
          )
      # segment.colour = "black",
      # # segment.size = 1.5)
  }

  if (!is_null(title)) p <- p + ggtitle(title)

    # Falls Dichtelinien, Legende plotten

    if (density) {
       p <-
           p +
            guides(size = "none") +
            theme(
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box.background = element_rect(
                    linetype = "solid",
                    colour = "gray17",
                    fill = "white"
                ),
                legend.text = element_text(size = 10),
                legend.box.margin = margin(0.07, 0.2, 0.1, 0, "cm"),
                legend.title = element_blank()
            )
    }

  # Plotten
  p
}
