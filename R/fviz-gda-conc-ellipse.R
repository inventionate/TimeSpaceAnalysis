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
#' @param scale_size scale minimal point size.
#' @param title the plot title.
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param density show density contours (boolean).
#' @param fill ellipse fill colour.
#' @param axis_lab_name name of axis label.
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#' @param xlim x Axis limits (vector of length 2).
#' @param ylim y Axis limits (vector of length 2).
#'
#' @return ggplot2 GDA visualisation with concentration ellipse.
#' @export
fviz_gda_conc_ellipse <- function(res_gda, level = 0.8647, alpha = 0.1, colour = "black", linetype = "dashed",
                                  density = FALSE, fill = NA, axes = 1:2, scale_size = 1,
                                  title = "GDA individuals plot", plot_modif_rates = TRUE, axis_lab_name = "Achse",
                                  labels = NULL, xlim = NULL, ylim = NULL) {

  if (!inherits(res_gda, c("MCA"))) {
    stop("Only MCA plots are currently supported!")
  }

  p <- .create_plot()

  p <-
    p +
    stat_ellipse(
      data = .count_distinct_ind(res_gda, axes),
      aes(x, y),
      geom = "polygon",
      level = level,
      type = "norm",
      alpha = alpha,
      colour = colour,
      fill = fill,
      linetype = linetype
    ) +
    geom_point(
      data = .count_distinct_ind(res_gda, axes) %>% distinct(),
      aes(x, y, size = count),
      inherit.aes = FALSE
    ) +
    scale_size_continuous(
      range = c(
        scale_size,
        scale_size * max(.count_distinct_ind(res_gda)$count)
      )
    )

  # 2D Density contours
  if (density) {
    p <-
      p +
      geom_density_2d(
        data = res_gda$ind$coord %>% as_data_frame,
        inherit.aes = FALSE, aes(`Dim 1`, `Dim 2`),
        colour = "gray"
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

  # Plot aufbereiten und finalisieren
  p <- .finalize_plot(
    plot = p,
    res_gda = res_gda,
    axes = axes,
    labels = labels,
    xlim = xlim,
    ylim = ylim
  )

  p <- .annotate_axes(p, labels)

  # Plotten
  p
}

