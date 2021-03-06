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
#' @param palette Colour brewer scale.
#' @param alpha_point opacity of individual points.
#' @param conc_linetype linetype of concentration ellipses.
#' @param conf_linetype linetype of confidence ellipses.
#' @param myriad use Myriad Pro font family (boolean).
#' @param concentration_ellipses plot concentration ellipse (boolean).
#' @param confidence_ellipses plot confidence ellipses (boolean).
#' @param conf_colour colour confidence ellipses (boolean).
#' @param impute impute missing data (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param ncol Number of facet columns.
#' @param individuals show individual points (boolean).
#' @param impute_ncp number of dimensions to predict missing values.
#' @param relevel character vector containing new level order.
#' @param alpha_ellipses concentration ellipses fill alpha.
#' @param plot_eta2 plot eta2 value per axis (boolean).
#' @param axis_lab_name name of axis label.
#' @param show_prop show prop label as mean point (boolean).
#'
#' @return ggplo2 visualization with concentration and quali var ellipses.
#' @export
fviz_gda_quali_ellipses <- function(res_gda, df_var_quali, var_quali, title = "MCA quali var ellipses",
                                    facet = TRUE, alpha_point = 0.75, conc_linetype = "solid", conf_linetype = "solid",
                                    scale_mean_points = TRUE, axes = 1:2, palette = "Set1", myriad = TRUE, impute = TRUE,
                                    concentration_ellipses = TRUE, confidence_ellipses = FALSE, conf_colour = FALSE,
                                    plot_modif_rates = TRUE, ncol = 3, individuals = TRUE, impute_ncp = 2, relevel = NULL,
                                    alpha_ellipses = 0.15, plot_eta2 = TRUE, axis_lab_name = "Achse", show_prop = FALSE) {

  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  # Datensatz auslesen
  var <- df_var_quali %>% select(!! var_quali) %>% mutate_all(funs(as.character))
  var_levels <- df_var_quali %>% select(!! var_quali) %>% mutate_all(funs(as.factor)) %>%
    magrittr::extract2(var_quali) %>% levels

  # Reihenfolge der Levels festlegen
  if( !is.null(relevel) ) {
    var_levels <- relevel
  }

  #eta2 extrahieren
  if ( plot_eta2 ) {

    # Berechnungen der passiven Variable durchführen
    supvar_stats <- supvar_stats(res_gda, df_var_quali, var_quali, impute, impute_ncp)

    supvar_eta2 <- bind_cols(rowname = rownames(supvar_stats$var), supvar_stats$var) %>%
      filter(rowname == "eta2") %>% select(-rowname) %>% mutate_all(funs(round(., 3)))

  } else {

    supvar_eta2 <- NULL

  }

  # Auf Fehlende Werte prüfen.
  exclude_na <- which(is.na(var))

  if( length(exclude_na) != 0 ) {

    if( impute ) {
      message("Info: Missing data will be imputed!")

      var <- var %>% mutate_all(funs(as.factor))

      # Nur aktive Individuen verwenden
      if(!is.null(res_gda$call$ind.sup)) X <- res_gda$call$X[-res_gda$call$ind.sup,]
      else X <- res_gda$call$X

      if(inherits(res_gda, c("MCA"))) {

        var_impute <- missMDA::imputeMCA(data.frame(X, var), ncp = impute_ncp)

      }

      if(inherits(res_gda, c("MFA"))) {

        var_impute <- missMDA::imputeMFA(data.frame(X, var),
                                         c(res_gda$call$group, 1),
                                         res_gda$call$ncp,
                                         c(res_gda$call$type, "n"),
                                         ncp = impute_ncp)

      }

      var <- var_impute$completeObs[var_quali]
    } else {
      # Fehlende Werte durch Kategorie ersetzen (falls nicht imputiert wurde).
      var[is.na(var)] <- "Fehlender Wert"
      var_levels <- c(var_levels, "Fehlender Wert")
    }

  }

  # Spalte in Vektor umwandeln
  var <- var %>% magrittr::extract2(var_quali)

  # Datensatz zusammenstellen (Koordinaten mit passiver Variable zusammenführen)
  df_source <- tibble(x = res_gda$ind$coord[, axes[1]], y = res_gda$ind$coord[, axes[2]], var_quali = factor(var)) %>%
    mutate(var_quali = factor(var_quali, levels = var_levels))

  coord_ind_quali <- df_source %>%
    group_by(x, y, var_quali) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    mutate(var_quali = fct_drop(var_quali),
           colour = as.character(as.numeric(var_quali)))

  coord_mean_quali <- coord_ind_quali %>% select(-colour) %>% group_by(var_quali) %>% summarise_all(funs(mean))
  size_mean_quali <- coord_ind_quali %>% select(-colour) %>% group_by(var_quali) %>% summarise_all(funs(length)) %>% ungroup() %>% mutate(size = count) %>% select(size) %>% data.frame
  coord_mean_quali <- bind_cols(coord_mean_quali, size = size_mean_quali) %>%
    mutate(prop = glue("{var_quali}: {round(size/sum(size) * 100, 1)}%"),
           colour = as.character(as.numeric(var_quali)))

  # Plot
  if(inherits(res_gda, c("MCA"))) p <- .create_plot()
  else stop("Only MCA plots are currently supported!")

  # ALlgemeine Konzentrationsellipse hinzufügen (level = 86,47% nach Le Roux/Rouanet 2010: 69, da es sich um eine 2-dimesnionale Konzentrationsellipse handelt)
  p <- p + stat_ellipse(data = .count_distinct_ind(res_gda), aes(x = x, y = y), geom ="polygon", level = 0.8647, type = "norm", alpha = 0.1, colour = "black", linetype = "dashed",
                        segments = 500, fill = "transparent")

  # Konzentrationsellipsen für die passiven Variablengruppen
  if(individuals) p <- p + geom_point(data = coord_ind_quali, aes(x = x, y = y, colour = colour, size = count), inherit.aes = FALSE, alpha = alpha_point)

  p <- p + scale_size_continuous(range = c(1, 7))

  if(concentration_ellipses) {

    ellipse_axes <- NULL
    for(i in seq_along(var_levels))
    {
      p_calc <- ggplot() + stat_ellipse(data = coord_ind_quali %>% filter(var_quali == var_levels[i]), aes(x, y), segments = 500, type = "norm", level = 0.86)

      # Get ellipse coords from plot
      pb = ggplot_build(p_calc)
      el = pb$data[[1]][c("x","y")]

      # Calculate centre of ellipse
      ctr = coord_mean_quali %>% filter(var_quali == var_levels[i]) %>% select(x, y) %>% as.matrix %>% as.vector

      # Calculate distance to centre from each ellipse pts
      dist2center <- sqrt(rowSums(t(t(el)-ctr)^2))

      # Identify axes points
      df <- bind_cols(el, dist2center = dist2center, var_quali = rep(var_levels[i], length(dist2center))) %>% arrange(dist2center) %>% slice(c(1, 2, n()-1, n())) %>% mutate(dist2center = round(dist2center, 2))

      # Store results
      ellipse_axes <- bind_rows(ellipse_axes, df) %>% mutate(group = paste(dist2center, var_quali))

    }

    if( !is.null(relevel) ) ellipse_axes %<>% mutate(var_quali = fct_relevel(var_quali, relevel))

    ellipse_axes %<>% mutate(colour = as.character(as.numeric(factor(var_quali, levels = levels(coord_ind_quali$var_quali)))))

    p <- p + stat_ellipse(data = coord_ind_quali, aes(x = x, y = y, fill = colour, colour = colour), geom ="polygon",  type = "norm", alpha = alpha_ellipses, linetype = conc_linetype, segments = 500, level = 0.8647, inherit.aes = FALSE) +
     geom_path(data = ellipse_axes, aes(x = x, y = y, group = group, colour = colour), linetype = "dashed", inherit.aes = FALSE)

  }

  if(confidence_ellipses) {
    conf_ellipses_coord <- FactoMineR::coord.ellipse(data.frame(coord_ind_quali[c(3,1,2)]), bary = TRUE)$res
    if(conf_colour) p <- p + geom_path(data = conf_ellipses_coord, aes(x, y, colour = colour), show.legend = FALSE, linetype = conf_linetype, size = 0.75)
    else p <- p + geom_path(data = conf_ellipses_coord, aes(x, y, group = colour), show.legend = FALSE, linetype = conf_linetype, size = 0.75)
  }

  if(scale_mean_points) p <- p + geom_point(data = coord_mean_quali, aes(x = x, y = y, fill = colour, size = size), colour = "black", shape = 23, inherit.aes = FALSE)
  else p <- p + geom_point(data = coord_mean_quali, aes(x = x, y = y, fill = colour), colour = "black", shape = 23, size = 10, inherit.aes = FALSE)

  if(show_prop) p <- p + geom_label(data = coord_mean_quali, aes(x = x, y = y, fill = colour, size = size, label = prop), colour = "black", inherit.aes = FALSE)

  if(palette != FALSE) p <- p + scale_colour_brewer(palette = palette) + scale_fill_brewer(palette = palette)

  if(facet) p <- p + facet_wrap(~var_quali, ncol = ncol)

  p <- add_theme(p) + ggtitle(title)

  # Beschriftung anpassen
  p <- .gda_plot_labels(res_gda, p, title, axes, plot_modif_rates, supvar_eta2, axis_lab_name = axis_lab_name)

  # Plotten
  p
}
