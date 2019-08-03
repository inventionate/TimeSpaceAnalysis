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
#' @param graph whether to plot or not to plot the praph (boolean).
#' @param print_place_duration print place overall duration (hours).
#' @param point_padding Amount of padding around labeled point. Defaults to unit(0, "lines").
#' @param open_sans use Open Sans font (boolean).
#' @param exclude_sleep exclude sleep duration (boolean).
#'
#' @return ggplot2 visualization of place chronology data.
#' @export
plot_places_chronology <- function(data,
                                   id,
                                   weekday = "all",
                                   size_range = NULL,
                                   colour_path = "black",
                                   size_path = 2,
                                   alpha_path = 0.25,
                                   linetype_path = "solid",
                                   force_repel = 3,
                                   title = NULL,
                                   axis_label = TRUE,
                                   xlim = NULL,
                                   ylim = NULL,
                                   graph = TRUE,
                                   print_place_duration = TRUE,
                                   point_padding = unit(1, "lines"),
                                   open_sans = TRUE,
                                   exclude_sleep = TRUE) {
  # Add Open Sans font family
  if (open_sans) .add_fonts()

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
      data = data_pc$data_unique_places_overall,
      aes(x = lon,
          y = lat,
          size = place_duration),
      show.legend = FALSE) +
    ggrepel::geom_label_repel(
      data = data_pc$data_unique_places_overall,
      aes(
        lon,
        lat,
        label = place,
        size = place_duration
      ),
      force = force_repel,
      colour = "white",
      fontface = "bold",
      family = "Fira Sans",
      fill = "black",
      show.legend = FALSE,
      segment.colour = "black",label.padding = 0.4) +
    coord_fixed(xlim = c(min(data_pc$data_places_chronology$lon)-0.03,
                         max(data_pc$data_places_chronology$lon)+0.23),
                ylim = c(min(data_pc$data_places_chronology$lat)-0.03,
                         max(data_pc$data_places_chronology$lat)+0.03)) +
    theme_void() +
    scale_x_continuous(expand = expand_scale(mult = c(0,0))) +
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

  if(!is.null(xlim)) plot_pc <- plot_pc + scale_x_continuous(limits = xlim)

  if(!is.null(ylim)) plot_pc <- plot_pc + scale_y_continuous(limits = ylim)

  # @TODO Integrate time structure and make x and yklim dynamic.

  # Time plot
  df_time <- get_places_chronology_time_pattern(data, id, weekday)

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
    scale_x_continuous(
      breaks = c(1:7),
      labels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"),
      name = "Wochentag",
      expand = expand_scale(mult = c(0,0))
      ) +
    scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0%", "25%", "50%", "75%", "100%"),
      name = "Zeitanteil in Prozent",
      expand = expand_scale(mult = c(0,0))
      ) +
    scale_fill_manual(
      name = "Tätigkeiten",
      values = colours
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x.bottom = element_text(family = "Fira Sans", face = "bold", size = 12),
      axis.text.y.left = element_text(family = "Fira Sans", size = 12),
      legend.text = element_text(family = "Fira Sans", size = 12),
      legend.title = element_text(family = "Fira Sans", face = "bold", size = 14),
      panel.border = element_rect(colour = "black", size = 1, fill = "transparent"),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.justification = c(1,0),
      panel.background = element_rect(fill = "transparent", colour = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent"), # get rid of leg
      rect = element_rect(fill = "transparent") # all rectangles
    ) +
    coord_fixed(ratio = 4, xlim = c(0.5,7.5), ylim = c(0,1))

  # COMBINE PLOTS -------------------------------------------------------------------------------

  plot_time_path <- tempfile(fileext = ".png")

  save_plot(plot_time_path, time, bg = "transparent")

  # Insert xbp_grob inside the scatter plot
  plot_pc_full <-
    ggdraw(plot_pc + theme_void()) +
    draw_image(plot_time_path, scale= 0.7, x = 0.35, y = 0.1)

  # Plotten
  if(graph) print(plot_pc_full)

  return(plot_pc_full)

}
