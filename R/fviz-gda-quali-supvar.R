#' @include utilities.R
#' @include supvar-stats.R
NULL
#' Visualize supplementary variables.
#'
#' @param res_gda GDA result.
#' @param df_var_quali crossed variable data frame.
#' @param var_quali crossed variable name.
#' @param title plot title.
#' @param path plot path (boolean).
#' @param linetype specify path linetype.
#' @param axes which axes should be plotted.
#' @param palette RColorBrewer palette.
#' @param scale_point scale points by weight (boolean).
#' @param scale_text scale text by weight (boolean).
#' @param size_point define point size.
#' @param size_text define text size.
#' @param impute impute missing data (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param impute_ncp number of dimensions to predict missing values.
#' @param relevel character vector containing new level order.
#' @param print_eta2 print eta2 value per axis (boolean).
#' @param axis_lab_name name of axis label.
#' @param axes_annotate_alpha alpha value of axes annotations.
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#' @param xlim x Axis limits (vector of length 2).
#' @param ylim y Axis limits (vector of length 2).
#' @param accuracy numeric vector (defaults to 0.1).
#' @param pos_adjust numeric vector for axis labels adjustment (defaults to 0.001)
#' @param colour_point should the points be coloured (boolean)?
#' @return ggplot2 visualization of supplementary variables.
#' @export
fviz_gda_quali_supvar <- function(res_gda, df_var_quali, var_quali, title = NULL, path = FALSE, linetype = "solid",
                                  axes = 1:2, scale_point = TRUE, size_point = 3, scale_text = FALSE, size_text = 3,
                                  palette = "Set1", impute = TRUE, plot_modif_rates = TRUE, impute_ncp = 2,
                                  relevel = NULL, print_eta2 = TRUE, axis_lab_name = "Achse", axes_annotate_alpha = 0.3,
                                  labels = NULL, xlim = NULL, ylim = NULL, accuracy = 0.1,
                                  pos_adjust = 0.001, colour_point = FALSE) {
  var <-
      df_var_quali %>%
      select(var_quali = {{ var_quali }}) %>%
      mutate_all(as.character)

  # Berechnungen der passiven Variable durchführen
  supvar_stats <- supvar_stats(
    res_gda,
    df_var_quali,
    {{ var_quali }},
    impute,
    impute_ncp
    )

  # Achsen auswählen
  supvar <-
    bind_cols(
      rowname = rownames(supvar_stats$coord),
      weight = supvar_stats$weight,
      supvar_stats$coord
    ) %>%
    select(
      rowname,
      weight,
      matches(str_glue("Dim.{axes[1]}$")),
      matches(str_glue("Dim.{axes[2]}$"))
    )

  #eta2 extrahieren
  if (print_eta2) {

      supvar_eta2 <-
          bind_cols(
              rowname = rownames(supvar_stats$var), supvar_stats$var
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

  {axes[1]}

  # Spaltennamen anpassen
  colnames(supvar) <- c("rowname", "weight", "Dim.1", "Dim.2")

  # Reihenfolge der Zeilen an die Faktorenlevels anpassen
  order_levels <-
    df_var_quali %>%
    select({{ var_quali }}) %>%
    as.data.frame()

  order_levels <- levels(factor(order_levels[,1]))

  if (!is.null(relevel)) order_levels <- relevel

  if (length(which(is.na(var))) != 0 & !impute) order_levels <- c(order_levels, "Fehlender Wert")

  supvar <- supvar %>% slice(match(order_levels, rowname))

  # Plot
  if (inherits(res_gda, c("MCA"))) {
    p <- .create_plot()
  } else {
    stop("Only MCA plots are currently supported!")
  }

  # Skalierungsgrenzen festlegen
  p <-
    p +
    scale_size_continuous(range = c(1, 7))

  # Punkte verbinden
  if (path) {
    p <-
      p +
      geom_path(
        data = supvar,
        aes(x = Dim.1, y = Dim.2),
        linetype = linetype
      )
  }

  # Punkte plotten
  mapping_point <-
      aes(
        x = Dim.1,
        y = Dim.2,
        fill = rowname,
        size = weight
    )

  if (!colour_point) mapping_point$fill <- NULL

  if (!scale_point) mapping_point$size <- NULL

  geom_mean_point <-
      geom_point(
        data = supvar,
        mapping = mapping_point,
        colour = "black",
        shape = 23,
        inherit.aes = FALSE
  )

  if (!colour_point) geom_mean_point$aes_params$fill <- "gray75"

  p <- p + geom_mean_point

  # Beschriftung hinzufügen
  if (scale_text) {
    p <-
      p +
      ggrepel::geom_text_repel(
        data = supvar,
        aes(x = Dim.1, y = Dim.2, size = weight, label = rowname),
        family = "Fira Sans Condensed",
        point.padding = unit(0.5, "lines")
      )
  } else {
    p <-
      p +
      ggrepel::geom_text_repel(
        data = supvar,
        aes(x = Dim.1, y = Dim.2, label = rowname),
        family = "Fira Sans Condensed",
        size = size_text,
        point.padding = unit(0.5, "lines")
      )
  }

  # Farbpalette wählen
  if (palette != FALSE) {
    p <-
      p +
      scale_colour_brewer(palette = palette) +
      scale_fill_brewer(palette = palette)
  }

  # Standardthema hinzufügen
  if (!is_null(title)) p <- p + ggtitle(title)

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
      xlim = xlim,
      ylim = ylim,
      accuracy = accuracy
  )

  # @TODO: FEINANPASSUNG DER LABELS, DA HIER OFT EIN SEHR NAHER ZOOM!
  p <- .annotate_axes(
      p,
      labels,
      alpha = axes_annotate_alpha,
      pos_adjust = pos_adjust
    )

  # Plotten
  p
}
