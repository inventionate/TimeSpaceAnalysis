#' @include utilities.R
#' @include get-places-chronology.R
NULL
#' Plot single or multiple place chronologies in different scales.
#'
#' @param data a data frame (columns: ID, day, duration, place, address, lon, lat, prop_duration).
#' @param id vector, which contains questionnaire ids. Choosa "all" to compute all ids.
#' @param weekday vector, which contains the weekday to plot.
#' @param size_range specify the size for visualizatipn of duration.
#' @param colour_path sepcify the path line colour.
#' @param size_path specify the path line size.
#' @param alpha_path specify the path line alpha value [0:1].
#' @param linetype_path specify the linetype of the path line.
#' @param force_repel specify how heavy the repel algorithmn should be.
#' @param xlim specify plot x limits.
#' @param ylim specify plot y limits.
#' @param title title of the plot.
#' @param axis_label show or hide axis labels (boolean).
#' @param print_place_duration print place overall duration (hours).
#' @param point_padding Amount of padding around labeled point. Defaults to unit(0, "lines").
#' @param exclude_sleep exclude sleep duration (boolean).
#' @param xextra extra space for time plot (units in cm).
#' @param alpha_points specify the point alpha value [0:1].
#' @param facets plot facets (boolean).
#'
#' @return ggplot2 visualization of place chronology data.
#' @export
plot_places_chronology <- function(data, id, weekday = "all", size_range = NULL, colour_path = "black", size_path = 2,
                                   alpha_path = 0.25, alpha_points = 0.85, linetype_path = "solid", force_repel = 3,
                                   title = NULL, axis_label = FALSE, xlim = NULL, ylim = NULL, xextra = 3,
                                   print_place_duration = TRUE, point_padding = unit(1, "lines"), exclude_sleep = TRUE,
                                   facets = FALSE) {
  # Check if only one id is given
  if (length(id) > 1) stop("Please give only one ID.")

  # Datensatz aufbereiten.
  data_pc <- get_places_chronology(data, id, weekday, title, exclude_sleep)

  if (print_place_duration & nrow(data_pc$data_unique_places_overall) > 0) {
    data_pc$data_unique_places_overall %>%
      select(questionnaire_id, place, place_duration) %>%
      mutate(place_duration =  round(place_duration, 2)) %>%
      arrange(questionnaire_id) %>%
      mutate(questionnaire_id = as.character(questionnaire_id)) %>%
      group_by(questionnaire_id, place) %>%
      spread(questionnaire_id, place_duration) %>%
      print(n = nrow(.))
  }

  # Adjust zoom box
  if (is.null(xlim)) {
    xlim = c(min(data_pc$data_places_chronology$lon) - 0.03,
             max(data_pc$data_places_chronology$lon) + 0.03)
  }

  if (is.null(ylim)) {
    ylim = c(min(data_pc$data_places_chronology$lat) - 0.03,
             max(data_pc$data_places_chronology$lat) + 0.03)
  }

  # Select data to plot
  if (facets) {
    df_pc <- data_pc$data_unique_places_overall_by_day
  } else {
    df_pc <- data_pc$data_unique_places_overall
  }

  # Geo plot
  plot_pc <-
    ggplot() +
    geom_path(
      data = data_pc$data_places_chronology,
      aes(x = lon, y = lat, group = day),
      size = size_path,
      alpha = alpha_path,
      lineend = "round",
      linetype = linetype_path,
      colour = colour_path) +
    geom_point(
      data = df_pc,
      aes(x = lon,
          y = lat,
          size = place_duration),
      show.legend = FALSE) +
    ggrepel::geom_label_repel(
      data = df_pc,
      aes(
        lon,
        lat,
        label = place,
        size = place_duration
      ),
      force = force_repel,
      colour = "white",
      fontface = "bold",
      family = "Fira Sans Condensed Medium",
      fill = "black",
      alpha = alpha_points,
      show.legend = FALSE,
      segment.colour = "black",label.padding = 0.4
    ) +
    coord_fixed(
      xlim = xlim,
      ylim = ylim
    ) +
    theme_void() +
    theme(
      axis.title = element_text(size = 10, family = "Fira Sans Condensed Medium"),
      axis.text = element_text(size = 8, family = "Fira Sans Condensed Medium"),
      plot.margin = unit(c(0, xextra, 0, 0),"cm"),
    ) +
    ggtitle(data_pc$title)

  if (!is.null(size_range)) {
    plot_pc <-
      plot_pc +
        scale_size(
          range = size_range,
          name = "Dauer",
          labels = function(x) paste0(x, "h")
        )
  }

  if (!axis_label) {
    plot_pc <-
      plot_pc +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  }

  if (facets) {
    plot_pc <-
      plot_pc +
      facet_wrap(~day, ncol = 4, nrow = 2) +
      theme(
        plot.margin = unit(c(0, 0, 0, 0),"cm"),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing.x=unit(1.5, "lines"),
        panel.spacing.y=unit(1, "lines"),
        panel.border = element_rect(
          fill = NA,
          colour = "black",
          size = 1,
          linetype = "solid"
        )
      )
  }

  # Time plot
  df_time <- get_places_chronology_time_pattern(data, id, weekday)

  colours <- c("#f15b60", "#ce7058", "#faa75b", "#9e67ab", "#5a9bd4", "#7ac36a", "#737373")
  # Die Farbe für "Lerngruppen" ändern, da es sich deutlich von "Zwischenzeit" unterscheiden sollte.
  colours[2] <- "#d77fb4"

  plot_time <-
    ggplot(
      df_time,
      aes(x = day, y = prop_duration)
      ) +
    geom_bar(
      aes(fill = activity),
      position = "fill",
      stat = "identity", width = 1
    ) +
    geom_vline(
      xintercept = c(1.5:6.5),
      linetype = "solid",
      colour = "white",
      size = 1
    ) +
    scale_x_discrete(
      labels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"),
      name = "Wochentag",
      expand = expansion(mult = c(0,0))
    ) +
    scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0%", "25%", "50%", "75%", "100%"),
      name = "Zeitanteil in Prozent",
      expand = expansion(mult = c(0,0))
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x.bottom = element_text(family = "Fira Sans Condensed Medium", face = "bold", size = 12),
      axis.text.y.left = element_text(family = "Fira Sans Condensed Medium", size = 12),
      legend.text = element_text(family = "Fira Sans Condensed Medium", size = 12),
      legend.title = element_text(family = "Fira Sans Condensed Medium", face = "bold", size = 14),
      panel.border = element_rect(colour = "black", size = 1, fill = NA),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.justification = c(1,0),
      panel.background = element_blank(), # bg of the panel
      plot.margin = margin(3, 3, 0, 3, "mm"),
      plot.background = element_blank(), # bg of the plot
      legend.background = element_blank(), # get rid of legend bg
      legend.box.background = element_rect(
        colour = "black",
        fill = NA,
        linetype = "solid"
      ), # get rid of leg
      rect = element_rect(fill = NA) # all rectangles
    ) +
    coord_fixed(ratio = 4, xlim = c(0.5,7.5), ylim = c(0,1), clip = "off")

  if (facets) {
    plot_time <-
      plot_time +
      scale_fill_manual(
        name = "Tätigkeiten",
        values = colours,
        guide = guide_legend(nrow = 3, ncol = 3)
      )
  } else {
    plot_time <-
      plot_time +
      scale_fill_manual(
        name = "Tätigkeiten",
        values = colours
      )
  }

  # COMBINE PLOTS -------------------------------------------------------------------------------

  # Save time plot
  plot_time_path <- tempfile(fileext = ".png")

  save_plot(plot_time_path, plot_time, bg = NA)

  # Combine places chronology and time plot
  if (facets) {
    plot_pc_full <-
      ggdraw(plot_pc) +
      draw_image(plot_time_path, scale = 0.4, x = 0.37, y = -0.21)
  } else {
    plot_pc_full <- plot_grid(plot_pc, plot_time, rel_widths = c(2, 1))
    # plot_pc_full <-
    #   ggdraw(plot_pc) +
    #   draw_image(plot_time_path, scale = 0.4, x = 0.35, y = 0.1)
  }

  # @TODO: Make legend direction editable and the plot position also.
  # try grid plot on right with legend vertical!

  plot_pc_full

}
