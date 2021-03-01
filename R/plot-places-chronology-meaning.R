#' @include utilities.R
#' @include get-places-chronology.R
NULL
#' Plot place chronologies map structure and zoom in.
#'
#' @param data a data frame (columns: ID, day, duration, place, address, lon, lat, prop_duration).
#' @param id vector, which contains questionnaire ids. Choose "all" to compute all ids.
#' @param weekday vector, which contains the weekday to plot.
#' @param size_range specify the size for visualizatipn of duration.
#' @param colour_path sepcify the path line colour.
#' @param size_path specify the path line size.
#' @param alpha_path specify the path line alpha value [0:1].
#' @param linetype_path specify the linetype of the path line.
#' @param title title of the plot.
#' @param axis_label show or hide axis labels (boolean).
#' @param print_place_duration print place overall duration (hours).
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
#' @param area_label_fontsize area label fontsize (vector).
#' @param map_add_x adjust map x area.
#' @param map_add_y adjust map y area.
#' @param exclude_na drop NA places (boolean).
#' @param map_scalebar show a scale bar (boolean).
#' @param map_scalebar_location location of the scalebar.
#' @param map_scalebar_text_size size of the scale text.
#' @param map_scalebar_box_size size of the box.
#' @param map_scalebar_border_size size of the border.
#' @param map_scalebar_dist displayed disctance.
#' @param map_scalebar_text_dist distance between box and text.
#' @param facets_include_place explicit include places in facets (vector).
#' @param facets_include_all include all place names in facet plot (boolean).
#' @param map_scalebar_unit_pos_dist add space between scalebar values and unit.
#' @param exclude_meaning meanings to be excluded (vector).
#' @param area_buffer The size of the region around the mark where labels cannot be placed (in mm).
#'
#' @return ggplot2 visualization of place chronology data.
#' @export
plot_places_chronology_meaning <- function(data, id, weekday = "all", size_range = NULL, colour_path = "black",
                                           size_path = 2, alpha_path = 0.25, alpha_points = 1, linetype_path = "solid",
                                           title = NULL, axis_label = FALSE, print_place_duration = TRUE,
                                           exclude_sleep = TRUE, facets = FALSE, facets_include_place = NULL,
                                           facets_include_all = FALSE, exclude_na = FALSE, exclude = NULL,
                                           exclude_meaning = NULL, meanings = NULL, map = FALSE, map_zoom = 10,
                                           map_add_x = 0.2, map_add_y = 0.1, graph = TRUE, area_fill = "white",
                                           area_colour = "black", area_alpha = 0, area_size = 1.5,
                                           area_linetype = "solid", area_label_fontsize = c(12, 10), area_buffer = 10,
                                           map_scalebar = TRUE, map_scalebar_location = "topright",
                                           map_scalebar_text_size = 4.5, map_scalebar_box_size = 0.015,
                                           map_scalebar_border_size = 0.85, map_scalebar_dist = 1,
                                           map_scalebar_text_dist = 0.02, map_scalebar_unit_pos_dist = 0.5) {

  # Check if only one id is given
  if (length(id) > 1) stop("Please give only one ID.")

  # Don't use maps in facets (free scale)
  if (map && facets) {
    map <- FALSE
    warning("You can't use map backgrounds in facets at the moment.", call. = FALSE)
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
      pivot_wider(
        names_from = questionnaire_id,
        names_glue = "duartion_{questionnaire_id}",
        values_from = place_duration
      ) %>%
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
        "{format(place_duration, decimal.mark = ',', digits = 1)} Stunden"
      )
    ) %>%
    # Exclude a whole meaningful plave
    filter(
      meaning %nin% exclude_meaning
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

  if (!is_null(exclude_meaning)) {
    warning("All places with no meaning will be dropped.", call. = FALSE)
    df_pc_meaning_path <-
      df_pc_meaning_path %>%
      drop_na()
  }

  if (exclude_na) {
    df_pc_meaning_path <-
      df_pc_meaning_path %>%
      drop_na()

    df_pc_meaning <-
      df_pc_meaning %>%
      drop_na()
  }

  # Prepare data for facets
  if (facets_include_all) {
    facets_include_place <- df_pc_meaning$place
  }

  df_pc_meaning_facets <-
    df_pc_meaning %>%
    filter(
      place_duration > mean(data_pc$data_unique_places_overall$place_duration) |
        place %in% facets_include_place
    )

  # Plot Stamen maps as background
  if (map) {
    height <- max(df_pc_meaning$lat) - min(df_pc_meaning$lat)
    width <- max(df_pc_meaning$lon) - min(df_pc_meaning$lon)
    borders <- c(
      bottom = min(df_pc_meaning$lat) - map_add_y * height,
      top = max(df_pc_meaning$lat) + map_add_y * height,
      left = min(df_pc_meaning$lon) - map_add_x * width,
      right = max(df_pc_meaning$lon) + map_add_x * width
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
        text = element_text(family = "Fira Sans Condensed Semibold")
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
    ggtitle(
      data_pc$title
    ) +
    theme(
      text = element_text(family = "Fira Sans Condensed Semibold"),
      title = element_text(face = "bold")
    )

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
        label.family = "Fira Sans Condensed Semibold",
        label.fontsize = area_label_fontsize,
        label.buffer = unit(area_buffer, "mm"),
        label.fill = "gray90",
        con.cap = unit(area_size, "mm"),
        show.legend = FALSE
      )
  } else {
    plot_pc <-
      plot_pc +
      geom_mark_circle(
        data = df_pc_meaning_facets,
        aes(
          x = lon,
          y = lat,
          group = place,
          label = place,
          description = place_desc
        ),
        expand = unit(0.1, "mm"),
        label.family = "Fira Sans Condensed Semibold",
        label.fontsize = area_label_fontsize,
        label.buffer = unit(10, "mm"),
        label.fill = "gray90",
        con.cap = unit(4, "mm"),
        show.legend = FALSE
      ) +
      theme(
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_text(
          face = "bold",
          family = "Fira Sans Condensed Semibold",
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
      ) +
      labs(
        caption = "Achtung: Die Skalierung der Grafiken ist unabhängig voneinander.
        Die Länge der Pfade ist nicht vergleichbar."
      )

    warning("The facets are free scaled. Use the plot only to investigate structure.", call. = FALSE)
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

  if (map && map_scalebar) {
    plot_pc <-
      plot_pc +
      scalebar(
        location = map_scalebar_location,
        dist = map_scalebar_dist,
        dist_unit = "km",
        transform = TRUE,
        model = "WGS84",
        st.size = map_scalebar_text_size,
        st.dist = map_scalebar_text_dist,
        height = map_scalebar_box_size,
        border.size = map_scalebar_border_size,
        x.min = min(df_pc_meaning$lon),
        x.max = max(df_pc_meaning$lon),
        y.min = min(df_pc_meaning$lat),
        y.max = max(df_pc_meaning$lat),
        unit_pos_dist = map_scalebar_unit_pos_dist
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
