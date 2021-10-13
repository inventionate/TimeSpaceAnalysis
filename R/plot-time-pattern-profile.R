#' @include utilities.R
#' @include get-time-pattern-profile.R
NULL
#' Plot average time pattern profiles.
#'
#' @param data_tp data frame containing questionnaire_id, kml3d results and time pattern data.
#' @param id time pattern to plot.
#' @param ncol facet columns.
#' @param fluid should be static bars or fluid lines visualized (boolean).
#'
#' @return ggplot2 avgerage time pattern profile plot.
#' @export
plot_time_pattern_profile <- function(data_tp, id = "all", ncol = 4, fluid = FALSE) {

  df_tp <- get_time_pattern_profile(data_tp, id)

  data_tsp <- df_tp$df_average_prop

  df_missing <-
    df_tp$df_profiles %>%
    filter(zeitmuster == "Fehlend")

  missing_cases = waiver()

  if (nrow(df_missing) > 0) {
    missing_cases = str_glue("Fehlende Fälle: {df_missing$prop}, n = {df_missing$n}")
  }

  # Überschrift der facets anpassen
  data_tsp <-
    data_tsp %>%
    mutate(
      zeitmuster = glue(
        "<b>{zeitmuster}</b><br>
        <span style='font-size:9pt'>{prop}, n = {n}<span>"
      ),
      activity = fct_recode(
        activity,
         "Arbeit" = "Arbeitszeit"
      )
    )

  colours <- c(
      "Lehrveranstaltungen" = "#f15b60",
      "Selbststudium"  = "#faa75b",
      "Lerngruppe" = "#CFAB59",
      "Zwischenzeit" = "#ce7058",
      "Fahrzeit" = "#9e67ab",
      "Arbeit" = "#5a9bd4",
      "Private Zeit" = "#7ac36a",
      "Schlafen" = "#737373"
  )

  colours <- colours[names(colours) %in% levels(data_tsp$activity)]

  p <-
    ggplot(
      data_tsp %>% drop_na(),
      aes(
        x = day,
        y = prop_avg_duration
      )
    )

  if (FALSE) {
    p <-
      p +
      geom_area(
        aes(fill = activity),
        position = "fill"
      ) +
      geom_vline(
        xintercept = c(1:7),
        linetype = "dotted",
        colour = "white"
      )
  } else {
    p <-
      p +
      geom_bar(
        aes(fill = activity),
        position = "fill",
        stat = "identity",
        width = 1
      ) +
      geom_vline(
        xintercept = c(1.5:6.5),
        linetype = "solid",
        colour = "white",
        size = 0.75
      )
  }

  p <-
    p +
    scale_x_continuous(
      breaks = c(1:7),
      labels = c("Mo",
                 "Di",
                 "Mi",
                 "Do",
                 "Fr",
                 "Sa",
                 "So"),
      name = "Wochentag",
      expand = expansion(mult = c(0,0))
    ) +
    scale_y_continuous(
      breaks = c(0,
                 0.25,
                 0.5,
                 0.75,
                 1),
      labels = c("0 %",
                 "25 %",
                 "50 %",
                 "75 %",
                 "100 %"),
      name = "Zeitanteil",
      expand = expansion(mult = c(0.005,0.005))
    ) +
    scale_fill_manual(
      name = "Tätigkeiten",
      values = colours
    )


  p <-
    p +
    theme_minimal(base_family = "Fira Sans Condensed Medium") +
    theme(
      text = element_text(colour = "gray17"),
      title = element_text(size = 14, colour = "gray17"),
      strip.text = element_textbox(size = 12, halign = 0.5, colour = "gray17"),
      panel.spacing.x = unit(3, "mm"),
      panel.spacing.y = unit(1, "lines"),
      axis.text = element_text(size = 9, colour = "gray17"),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(
        fill = NA,
        colour = "gray17",
        size = 1,
        linetype = "solid"
      ),
      plot.margin = margin(0.75, 0, 0, 0, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.caption.position = "plot",
      plot.caption = element_text(vjust = 19)
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(caption = missing_cases) +
    coord_fixed(ratio = 4)


  # Mehrere Gafiken parallel erzeugen
  p <- p + facet_wrap(~zeitmuster, ncol = 4)

  p <- p + coord_fixed(5)

  p
}
