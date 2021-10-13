#' @include utilities.R
#' @include get-places-chronology-time-pattern.R
NULL
#' Plot place chronology time pattern data.
#'
#' @param data data frame, which contains place chronology time pattern data.
#' @param id vector, which contains questionnaire ids. Use "all" if you want to plot all ids.
#' @param weekday weekday vector, which contains days to plot.
#' @param graph whether to plot or not to plot the praph (boolean)
#' @param print_prop_duration whether to print or not to print prop duration data.
#' @param legend show or hide legends (boolean).
#' @param bar_width specify the width of the bars.
#' @param ncol number of cols, if there are multiple plots (facets).
#' @param labels facet labels.
#' @param facet_label show facets (boolean).
#' @param legend_bottom show legend on bottom (boolean).
#' @param legend_cols number of legend cols (numeric).
#'
#' @return ggplot2 visualization of place chronology time pattern data.
#' @export
plot_places_chronology_time_pattern <- function(data, id = "all", weekday = "all", graph = TRUE,
                                                print_prop_duration = TRUE, legend = TRUE,
                                                bar_width = 1, ncol = 3, labels = NULL,
                                                facet_label = TRUE, legend_bottom = TRUE,
                                                legend_cols = 2) {

  # Datensatz Zeitmuster
  data_pc_zm <- get_places_chronology_time_pattern(data, id, weekday)

  # Kontrolle, ob es Lerngruppe gibt und ggf. entfernen.

  # Lerngruppe aus dem Datensatz entfernen und auch die Farben entsprechend anpassen!
  #     Außerdem andere Angabe für das allgemeine Datensätzchen

  # Tagesauswahl definieren
  if (weekday[[1]] != "all") {
    # Farbpalette festlegen
      colours <- c(
          "Lehrveranstaltung" = "#f15b60",
          "Selbststudium"  = "#faa75b",
          "Lerngruppe" = "#CFAB59",
          "Zwischenzeit" = "#ce7058",
          "Fahrzeit" = "#9e67ab",
          "Arbeit" = "#5a9bd4",
          "Private Zeit" = "#7ac36a",
          "Schlafen" = "#737373"
      )

      colours <- colours[names(colours) %in% levels(data_pc_zm$activity)]


    # Prozentuale Verteilung der Aktivitäten
    if(print_prop_duration) {
      data_pc_zm %>%
        select(questionnaire_id, activity, prop_duration) %>%
        mutate(prop_duration =  round(prop_duration * 100, 2)) %>%
        arrange(questionnaire_id) %>%
        group_by(questionnaire_id, activity) %>%
        spread(questionnaire_id, prop_duration) %>%
        as_tibble() %>%
        print(n = nrow(.))
      }

      # Plotten der Zeitmuster
      plot_pc_zm <-
        ggplot(data_pc_zm, aes(x = day, y = prop_duration)) +
        geom_bar(
          aes(fill = activity),
          stat = "identity",
          position = "stack",
          width = bar_width
        ) +
        scale_y_continuous(
          breaks = c(0, 0.25, 0.5, 0.75, 1),
          labels = c("0", "25", "50", "75", "100")
        ) +
        #scale_fill_brewer(name = "Tätigkeiten:",
        #                 labels = c("Veranstaltungen", "Zwischenzeit", "Selbststudium", "Fahrzeit", "Arbeitszeit", "Freizeit", "Schlafen"),
        #                palette = "Spectral") +
        scale_fill_manual(
          name = "Tätigkeiten",
          values = colours,
          guide = guide_legend(reverse=TRUE)
        )

      if (length(id) > 1 | id[[1]] == "all") {
        plot_pc_zm <-
          plot_pc_zm +
          facet_wrap(
            ~questionnaire_id,
            ncol = ncol,
            labeller = as_labeller(labels)
          )
      }

    } else {
      # Allgemeines Zeitmuster plotten.
      plot_pc_zm <-
        plot_time_pattern(
          data_pc_zm,
          id = id,
          reshape_data = FALSE,
          ncol = ncol,
          print_prop_duration = print_prop_duration,
          labels = labels
        )
    }

    # Theme
    plot_pc_zm <-
      plot_pc_zm +
      theme_minimal(base_family = "Fira Sans Condensed Medium") +
      theme(
        text = element_text(colour = "gray17"),
        title = element_text(size = 20),
        strip.text = element_text(size = 20, face = "bold"),
        panel.spacing.x = unit(3, "mm"),
        panel.spacing.y = unit(1, "lines"),
        axis.text = element_text(size = 15),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          fill = NA,
          colour = "gray17",
          size = 2,
          linetype = "solid"
        ),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "right"
      )
      coord_fixed(ratio = 4, clip = "off")

    if (!legend) {
        plot_pc_zm <-
            plot_pc_zm +
            theme(legend.position = "none")
    }

    if (!facet_label) {
        plot_pc_zm <-
            plot_pc_zm +
            theme(strip.text = element_blank())
    }

    if (legend_bottom) {
        plot_pc_zm <-
            plot_pc_zm +
            theme(legend.position = "bottom") +
            guides(fill = guide_legend(ncol = legend_cols))
    }

    if (graph) print(plot_pc_zm)

  plot_pc_zm
}
