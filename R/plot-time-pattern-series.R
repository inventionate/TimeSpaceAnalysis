#' @include utilities.R
#' @include get-time-pattern-series.R
NULL
#' Plot time pattern series data.
#'
#' @param data_tp data frame including questionnaire_id, kml3d results and time pattern data.
#' @param alpha opacity of the time pattern lines.
#' @param colours colour of average time pattern lines.
#' @param title plot title.
#' @param hour_scale y axis breaks (hours).
#' @param hour_limits y axis limits (hours).
#' @param individual_lines show individual time pattern lines (boolean).
#'
#' @return ggplot2 time pattern series plot.
#' @export
plot_time_pattern_series <- function(data_tp, alpha = 0.3, individual_lines = FALSE, colours = NULL,
                                     title = "Time pattern profiles (kml3d results)", hour_limits = c(0, 24),
                                     hour_scale = c(0, 4, 8, 12)) {
  data_ts <- get_time_pattern_series(data_tp)

  if (is.null(colours)) {
    colours = RColorBrewer::brewer.pal(9, "Set1")
  }

  # Zeitserien plotten
  p <-
    ggplot(
      data_ts$data_series,
      aes(
        x = day,
        y = duration,
        group = questionnaire_id
      )
    )
  # Das hier optional machen
  if (individual_lines) {
    p <- p + geom_line(alpha = alpha)
  }
  p <-
    p +
    facet_wrap(~activity, nrow = 2, scales = "free") +
    geom_line(
      data = data_ts$data_series_average,
      aes(
        x = day,
        y = avg_duration,
        group = as.factor(zeitmuster),
        colour = as.factor(zeitmuster)
      ),
      inherit.aes = FALSE,
      size = 1
    ) +
    geom_point(
      data = data_ts$data_series_average,
      aes(
        x = day,
        y = avg_duration,
        group = as.factor(zeitmuster),
        colour = as.factor(zeitmuster)
      ),
      shape = 15,
      inherit.aes = FALSE,
      size = 3
    ) +
    scale_colour_manual(
      values = colours,
      name = "Zeitmuster",
      labels = data_ts$data_series_profile_prop_label
    ) +
    scale_x_discrete(
      name = "Wochentage",
      expand = c(0.1, 0)
    ) +
    scale_y_continuous(
      limits = hour_limits,
      breaks = hour_scale,
      labels = paste(c(0, 4, 8, 12), "h"),
      name = "Dauer (in Stunden)"
    ) +
    ggtitle(title) +
    theme_minimal_hgrid(font_family = "Fira Sans Condensed Medium") +
    theme(
      strip.text = element_text(size = 15, face = "bold"),
      axis.line.x = element_blank(),
      axis.title = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(10, "mm"),
      legend.position = c(0.78, 0.2),
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )

  p
}
