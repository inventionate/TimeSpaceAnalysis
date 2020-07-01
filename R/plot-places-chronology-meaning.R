#' @include utilities.R
#' @include get-places-chronology.R
NULL
#' Plot place chronologies map structure and zoom in.
#'
#' @param data a data frame (columns: ID, day, duration, place, address, lon, lat, prop_duration).
#' @param id vector, which contains questionnaire ids. Choosa "all" to compute all ids.
#' @param weekday vector, which contains the weekday to plot.
#' @param size_range specify the size for visualizatipn of duration.
#' @param colour_path sepcify the path line colour.
#' @param size_path specify the path line size.
#' @param alpha_path specify the path line alpha value [0:1].
#' @param linetype_path specify the linetype of the path line.
#' @param title title of the plot.
#' @param axis_label show or hide axis labels (boolean).
#' @param print_place_duration print place overall duration (hours).
#' @param open_sans use Open Sans font (boolean).
#' @param exclude_sleep exclude sleep duration (boolean).
#' @param alpha_points specify the point alpha value [0:1].
#' @param facets plot facets (boolean).
#' @param exclude exclude specific places from the plot (vector).
#' @param meanings give places a meaning for grouping (vector).
#' @param graph plot graph (boolean).
#' @param map use map background (boolean).
#' @param map_zoom map zoom level.
#' @param area_fill fill colour of meaning area.
#' @param area_colour line colour of meaning area.
#' @param area_alpha alpha of meaning area.
#' @param area_size size of meaning area.
#' @param area_linetype linetype of meaning area.
#' @param facets_na auto remove places without meanings in facets (boolean).
#' @param area_label_fontsize area label fontsize (vector).
#'
#' @return ggplot2 visualization of place chronology data.
#' @export
plot_places_chronology_meaning <- function(data,
                                           id,
                                           weekday = "all",
                                           size_range = NULL,
                                           colour_path = "black",
                                           size_path = 2,
                                           alpha_path = 0.25,
                                           alpha_points = 1,
                                           linetype_path = "solid",
                                           title = NULL,
                                           axis_label = FALSE,
                                           print_place_duration = TRUE,
                                           open_sans = TRUE,
                                           exclude_sleep = TRUE,
                                           facets = FALSE,
                                           facets_na = FALSE,
                                           exclude = NULL,
                                           meanings = NULL,
                                           map = FALSE,
                                           map_zoom = 10,
                                           graph = TRUE,
                                           area_fill = "white",
                                           area_colour = "black",
                                           area_alpha = 0,
                                           area_size = 2,
                                           area_linetype = "solid",
                                           area_label_fontsize = c(12, 10)) {
  # Add Open Sans font family
  if (open_sans) .add_fonts()

  # Check if only one id is given
  if (length(id) > 1) stop("Please give only one ID.")

  # Don't use maps in facets (free scale)
  if (map && facets) {
    map <- FALSE
    warning("You can't use map backgrounds in facets at the moment.")
  }

  # Datensatz aufbereiten.
  data_pc <- get_places_chronology(data, id, weekday, title, exclude_sleep)

  # Check if there are meanings
  if (is_null(meanings)) {
    print("Given places:", quote = FALSE)
    print(data_pc$data_unique_places_overall$place %>% as.character())
    stop("Please choose place meanings.")

  }

  # Check if the given meanings fit
  if (length(data_pc$data_unique_places_overall$place) != length(meanings)) {
    stop("You have to choose exactly one meaning per place!")
  }

  if (print_place_duration & nrow(data_pc$data_unique_places_overall) > 0) {
    data_pc$data_unique_places_overall %>%
      select(questionnaire_id, place, place_duration) %>%
      mutate(place_duration = round(place_duration, 2)) %>%
      arrange(questionnaire_id) %>%
      mutate(questionnaire_id = as.character(questionnaire_id)) %>%
      group_by(questionnaire_id, place) %>%
      spread(questionnaire_id, place_duration) %>%
      print(n = nrow(.))
  }

  # Create meaning tibble
  df_pc_meaning <-
    data_pc$data_unique_places_overall %>%
    bind_cols(
      meaning = meanings
    ) %>%
    filter(place %nin% exclude) %>%
    mutate(
      place_desc = str_glue(
        "{format(place_duration, decimal.mark = ',')} Stunden"
      )
    )

  # Create meanings path tibble
  df_pc_meaning_path <-
    data_pc$data_places_chronology %>%
    filter(place %nin% exclude) %>%
    left_join(
      .,
      df_pc_meaning %>% select(place, meaning),
      by = "place"
    )

  if (!facets_na) {
    df_pc_meaning_path <-
      df_pc_meaning_path %>%
      drop_na()

    df_pc_meaning <-
      df_pc_meaning %>%
      drop_na()
  }

  # Plot Stamen maps as background
  if (map) {
    height <- max(df_pc_meaning$lat) - min(df_pc_meaning$lat)
    width <- max(df_pc_meaning$lon) - min(df_pc_meaning$lon)
    borders <- c(
      bottom = min(df_pc_meaning$lat) - 0.1 * height,
      top = max(df_pc_meaning$lat) + 0.1 * height,
      left = min(df_pc_meaning$lon) - 0.2 * width,
      right = max(df_pc_meaning$lon) + 0.2 * width
    )

    map_background <-
      get_stamenmap(
        bbox = borders,
        zoom = map_zoom,
        maptype = "watercolor",
    )

    plot_pc <- ggmap(map_background)
  } else {
    plot_pc <- ggplot() +
      theme_void() +
      theme(
        text = element_text(family = "Fira Sans Condensed")
      )
  }

  if (!map && !facets) {
    plot_pc <-
      plot_pc +
      coord_quickmap() +
      scale_y_continuous(expand = expansion(add = 0.1)) +
      scale_x_continuous(expand = expansion(add = 0.1))
  }

  plot_pc <-
    plot_pc +
    geom_path(
      data = df_pc_meaning_path,
      aes(
        x = lon,
        y = lat,
        group = day
      ),
      size = size_path,
      alpha = alpha_path,
      lineend = "round",
      linetype = linetype_path,
      colour = "black"
    ) +
    geom_point(
      data = df_pc_meaning,
      aes(
        x = lon,
        y = lat,
        size = place_duration
      ),
      alpha = alpha_points,
      show.legend = FALSE
    ) +
    ggtitle(data_pc$title)

  # Geo plot
  if (!facets) {
    plot_pc <-
      plot_pc +
      ggforce::geom_mark_hull(
        data = df_pc_meaning %>% drop_na(),
        aes(
          x = lon,
          y = lat,
          group = meaning,
          label = meaning
        ),
        expand = unit(5, "mm"),
        radius = unit(5, "mm"),
        size = area_size,
        fill = area_fill,
        alpha = area_alpha,
        colour = area_colour,
        linetype = area_linetype,
        label.family = "Fira Sans Condensed",
        label.fontsize = area_label_fontsize,
        label.buffer = unit(10, "mm"),
        label.fill = "gray90",
        con.cap = unit(area_size, "mm"),
        show.legend = FALSE
      )
  } else {
    plot_pc <-
      plot_pc +
      geom_mark_circle(
        data = df_pc_meaning %>% filter(place_duration > mean(place_duration)),
        aes(
          x = lon,
          y = lat,
          group = place,
          label = place,
          description = place_desc
        ),
        expand = unit(0.1, "mm"),
        label.family = "Fira Sans Condensed",
        label.fontsize = c(12, 10),
        label.buffer = unit(10, "mm"),
        label.fill = "gray90",
        con.cap = unit(4, "mm"),
        show.legend = FALSE
    ) +
    theme(
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_text(
          face = "bold",
          family = "Fira Sans Condensed",
          size = 20,
          margin = margin(b = 5)
        ),
        panel.spacing.x = unit(1, "cm"),
        panel.border = element_rect(
          fill = "transparent",
          size = 1
        )
      ) +
      scale_size(
        range = c(4, 8)
      ) +
      facet_wrap(
        ~meaning,
        scales = "free"
      )
  }

  if (!is_null(size_range)) {
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

  if (graph) print(plot_pc)

  plot_pc
}
