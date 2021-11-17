#' @include utilities.R
#' @include get-time-pattern-series.R
NULL
#' Plot time pattern series data.
#'
#' @param data_tp data frame including questionnaire_id, kml3d results and time pattern data.
#' @param alpha opacity of the time pattern lines.
#' @param title plot title.
#' @param hour_scale y axis breaks (hours).
#' @param hour_limits y axis limits (hours).
#' @param individual_lines show individual time pattern lines (boolean).
#'
#' @return ggplot2 time pattern series plot.
#' @export
plot_time_pattern_series <- function(data_tp, alpha = 0.3, individual_lines = FALSE,
                                     title = "Time pattern profiles (kml3d results)",
                                     hour_limits = c(0, 24), hour_scale = c(0, 4, 8, 12)) {
  data_ts <- get_time_pattern_series(data_tp)

  colours <- c(
      "Zeitmuster: Lehrveranstaltung" = "#f15b60",
      "Zeitmuster: Selbststudium" = "#faa75b",
      "Zeitmuster: Lerngruppe" = "#CFAB59",
      "Zeitmuster: Zwischenzeit" = "#ce7058",
      "Zeitmuster: Pendeln" = "#9e67ab",
      "Zeitmuster: Erwerbsarbeit" = "#5a9bd4",
      "Zeitmuster: Private Zeit" = "#7ac36a",
      "Zeitmuster: Schlafen" = "#737373"
  )

  colours <- colours[
      names(colours) %in% levels(data_ts$data_series_average$zeitmuster)
    ]

  # Reihenfolge abgleichen, um die korrekten Farben zu bestimmen.
  colours <- colours[
      order(factor(names(colours),
                   levels=levels(data_ts$data_series_average$zeitmuster))
            )
      ]

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
        group = zeitmuster,
        colour = zeitmuster
      ),
      inherit.aes = FALSE,
      size = 1
    ) +
    geom_point(
      data = data_ts$data_series_average,
      aes(
        x = day,
        y = avg_duration,
        group = zeitmuster,
        colour = zeitmuster
      ),
      shape = 15,
      inherit.aes = FALSE,
      size = 3
    ) +
    scale_colour_manual(
        name = "Zeitmuster",
        values = colours,
        labels = data_ts$data_series_profile_prop_label$zeitmuster
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
