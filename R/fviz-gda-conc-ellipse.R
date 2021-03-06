#' @include utilities.R
NULL
#' Title
#'
#' @param res_gda GDA result.
#' @param level ellipse level (default 86.47\%).
#' @param alpha opacity level (default 0.1).
#' @param colour ellipse border colour.
#' @param linetype ellipse edge linetype.
#' @param axes the GDA dimensions to plot.
#' @param myriad use Myriad Pro font (boolean).
#' @param scale_size scale minimal point size.
#' @param title the plot title.
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param density show density contours (boolean).
#' @param fill ellipse fill colour.
#' @param axis_lab_name name of axis label.
#'
#' @return ggplot2 GDA visualisation with concentration ellipse.
#' @export
fviz_gda_conc_ellipse <- function(res_gda, level = 0.8647, alpha = 0.1, colour = "black", linetype = "dashed", density = FALSE, fill = "gray",
                                  axes = 1:2, myriad = TRUE, scale_size = 1, title = "GDA individuals plot", plot_modif_rates = TRUE,
                                  axis_lab_name = "Achse") {

  # Add Myriad Pro font family
  if( myriad ) .add_fonts()

  if(inherits(res_gda, c("MCA"))) p <- .create_plot()
  else stop("Only MCA plots are currently supported!")

  p <- p + stat_ellipse(data = .count_distinct_ind(res_gda, axes), aes(x, y), geom ="polygon",
                        level = level, type = "norm", alpha = alpha, colour = colour, fill = fill,
                        linetype = linetype) +
    geom_point(data = .count_distinct_ind(res_gda, axes) %>% distinct(),
               aes(x, y, size = count), inherit.aes = FALSE) +
    scale_size_continuous(range = c(scale_size, scale_size * max(.count_distinct_ind(res_gda)$count)))

  # Theme adaptieren
  p <- add_theme(p)

  # 2D Density contours
  if( density ) p <- p + geom_density_2d(data = res_gda$ind$coord %>% as_data_frame, inherit.aes = FALSE, aes(`Dim 1`, `Dim 2`), colour = "gray")

  # Beschriftung anpassen
  p <- .gda_plot_labels(res_gda, p, title, axes, plot_modif_rates, axis_lab_name = axis_lab_name)

  # Plotten
  return(p)
}

