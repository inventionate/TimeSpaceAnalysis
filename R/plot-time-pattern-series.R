#' @include utilities.R
#' @include get-time-pattern-series.R
NULL
#' Plot time pattern series data.
#'
#' @param data_tp data frame including questionnaire_id, kml3d results and time pattern data.
#' @param alpha opacity of the time pattern lines.
#' @param palette colour of average time pattern lines.
#' @param open_sans use Open Sans font (boolean).
#' @param title plot title.
#' @param hour_scale y axis breaks (hours).
#' @param hour_limits y axis limits (hours).
#'
#' @return ggplot2 time pattern series plot.
#' @export
plot_time_pattern_series <- function(data_tp,
                                     alpha = 0.3,
                                     palette = "Set1",
                                     open_sans = TRUE,
                                     title = "Time pattern profiles (kml3d results)",
                                     hour_limits = c(0,24),
                                     hour_scale = c(0, 4, 8, 12, 16, 20, 24)) {

    # Add Open Sans font family
  if (open_sans) .add_fonts()

  data_ts <- get_time_pattern_series(data_tp)

  # Zeitserien plotten
  p <-
    ggplot(
      data_ts$data_series,
      aes(
        x = day,
        y = duration,
        group = questionnaire_id
        )
      ) +
    geom_line(alpha = alpha) +
    facet_wrap(~activity) +
    geom_line(
      data = data_ts$data_series_average,
      aes(
        x = day,
        y = avg_duration,
        group = as.factor(zeitmuster),
        colour = as.factor(zeitmuster)
        ),
      inherit.aes = FALSE,
      size = 2) +
    scale_colour_brewer(
      palette = palette,
      name = "Zeitmuster",
      labels = data_ts$data_series_profile_prop_label
      ) +
    scale_x_discrete(
      name = "Wochentage",
      expand = expand_scale(mult = c(0,0))
      ) +
    scale_y_continuous(
      limits = hour_limits,
      breaks = hour_scale,
      name = "Dauer (in Stunden)"
      ) +
    ggtitle(title) +
    theme_minimal(base_family = "Fira Sans") +
    theme(
      title = element_text(size = 14),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 12),
      axis.ticks = element_line(size = 0.5, colour = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(
        fill = NA,
        colour = "black",
        size = 1,
        linetype = "solid"),
      legend.position = "bottom",
      legend.title = element_blank()
    )

  p
}
