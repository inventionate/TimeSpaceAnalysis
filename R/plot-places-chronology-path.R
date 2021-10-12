#' @include utilities.R
#' @include get-places-chronology.R
NULL
#' Plot place chronologies activity paths.
#'
#' @param data a data frame (columns: ID, day, duration, place, address, lon, lat, prop_duration).
#' @param id vector, which contains questionnaire ids. Choose "all" to compute all ids.
#' @param recodeded_places recode leves of place labels by named vector.
#' @param week recode leves of week labels by named vector.
#'
#' @return ggplot2 visualization of place chronology path.
#' @export
plot_places_chronology_path <- function(data, id, recodeded_places = NULL,
                                        recode_week = c("Woche 4" = "5", "Woche 3" = "4",
                                                        "Woche 2" = "3", "Woche 1" = "2")
                                        ) {
      # Check if only one id is given
      if (length(id) > 1) stop("Please give only one ID.")

    # Datensatz aufbereiten.
    df_pc_prepared <-
        get_places_chronology(
            data = data,
            id = id,
            weekday = "all",
            title = "",
            exclude_sleep = FALSE
        )

    # Create meanings path tibble
    df_pc_path <-
        df_pc_prepared$data_places_chronology %>%
        ungroup() %>%
        add_row(
            questionnaire_id = df_pc_prepared$data_places_chronology[1, ]$questionnaire_id,
            start_time = 0,
            duration = df_pc_prepared$data_places_chronology[1, ]$start_time,
            date = df_pc_prepared$data_places_chronology[1, ]$date,
            place = df_pc_prepared$data_places_chronology[1, ]$place,
            activity = "Schlafen",
            .before = 1
        ) %>%
        # Rename weeks
        mutate(
            week = fct_relevel(
                fct_recode(
                    as_factor(lubridate::isoweek(date)),
                    !!!recode_week
                ),
                "Woche 4", "Woche 3", "Woche 2", "Woche 1"
            ),
            date = lubridate::wday(date, TRUE, week_start = 1)
        ) %>%
        filter(questionnaire_id == id) %>%
        filter(duration > 0) %>%
        mutate(
            place = fct_recode(
                place,
                !!!recodeded_places
            ),
            activity = fct_recode(
                activity,
                "Arbeit" = "Arbeitszeit",
                "Private Zeit" = "Freizeit",
            ),
            activity = fct_relevel(
                activity,
                "Lehrveranstaltung",
                "Lerngruppe",
                "Selbststudium",
                "Fahrzeit",
                "Arbeit",
                "Private Zeit",
                "Schlafen"
            ),
            place_label = if_else(
                place == lag(place) & start_time != 0,
                NA_character_,
                as.character(place)
            )
        ) %>%
        select(questionnaire_id, start_time, duration, date, place, activity, week, place_label)

    # Farben definieren
    colours <- c(
        "Lehrveranstaltung" = "#f15b60",
        "Selbststudium"  = "#faa75b",
        "Lerngruppe" = "#d77fb4",
        "Arbeit" = "#5a9bd4",
        "Private Zeit" = "#7ac36a",
        "Schlafen" = "#737373"
    )

    colours <- colours[names(colours) %in% levels(df_pc_path$activity)]

    p <-
        ggplot(
            df_pc_path,
            aes(x = date, y = start_time)
        ) +
        geom_text_repel(
            data = df_pc_path %>% drop_na(),
            aes(label = place_label),
            angle = 0,
            nudge_x = -0.1,
            nudge_y = 0.5,
            hjust = "right",
            family = "Fira Sans Condensed",
            direction = "y",
            max.overlaps = Inf
        ) +
        geom_vline(aes(xintercept = date), linetype = "solid", size = 1.3) +
        geom_segment(aes(xend = date, yend = start_time + duration), size = 4) +
        geom_segment(aes(xend = date, yend = start_time + duration, colour = activity), size = 2) +
        scale_y_continuous(
            breaks = c(0, 6, 12, 18, 24),
            expand = expansion(add = 0, mult = 0)
        ) +
        scale_x_discrete(expand = expansion(add = c(1.5, 0.5))) +
        coord_cartesian(ylim = c(0, 24)) +
        cowplot::theme_minimal_hgrid(font_family = "Fira Sans Condensed") +
        cowplot::panel_border(colour = "black") +
        ylab("") +
        xlab("") +
        theme(
            legend.position = "bottom",
            legend.justification = "center",
            legend.title = element_blank(),
            strip.text = element_text(
                face = "bold",
                size = 16,
                hjust = 0.5,
                margin = margin(0,0,4,0)
            ),
            legend.margin = margin(0,0,0,0),
            legend.box.margin = margin(-20,0,0,0)
        ) +
        scale_colour_manual(values = colours) +
        facet_wrap(~week, ncol = 1)

    p
}
