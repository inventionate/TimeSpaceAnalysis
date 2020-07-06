#' @include utilities.R
#' @include get-time-pattern-profile.R
NULL
#' Plot average time pattern profiles.
#'
#' @param data_tp data frame containing questionnaire_id, kml3d results and time pattern data.
#' @param id time pattern to plot.
#' @param ncol facet columns.
#' @param open_sans use Open Sans font family (boolean).
#' @param fluid should be static bars or fluid lines visualized (boolean).
#'
#' @return ggplot2 avgerage time pattern profile plot.
#' @export
plot_time_pattern_profile <- function(data_tp,
                                      id = "all",
                                      ncol = 4,
                                      fluid = FALSE,
                                      open_sans = TRUE) {
  # Add Open Sans font family
  if (open_sans) .add_fonts()

  data_tsp <- get_time_pattern_profile(data_tp, id)

  # Überschrift der facets anpassen
  data_tsp <-
    data_tsp %>%
    mutate(
      zeitmuster = glue(
        "<b>{zeitmuster}</b><br>
        <span style='font-size:9pt'>{prop}, n = {n}<span>"
      )
    )

  # Fixe sieben Kategorien
  colours <- c("#f15b60",
               "#ce7058",
               "#faa75b",
               "#9e67ab",
               "#5a9bd4",
               "#7ac36a",
               "#737373")

  p <-
    ggplot(
      data_tsp,
      aes(
        x = day,
        y = prop_avg_duration
      )
    )

  if (fluid) {
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
    theme_minimal(base_family = "Fira Sans Condensed") +
    theme(
      text = element_text(colour = "gray17"),
      title = element_text(size = 14, colour = "gray17"),
      strip.text = element_textbox(size = 12, halign = 0.5, colour = "gray17"),
      panel.spacing.x = unit(1.5, "lines"),
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
      legend.direction = "horizontal"
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    +
    geom_hline(
      yintercept = 0.5,
      size = 1,
      colour = "gray85",
      linetype = "dotted"
    ) +
    coord_fixed(ratio = 4)

  # Mehrere Gafiken parallel erzeugen
  p <- p + facet_wrap(~zeitmuster, ncol = ncol)

  p <- p + coord_fixed(5)

  p
}
