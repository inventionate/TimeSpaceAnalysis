#' Reshpape place chronology time pattern data.
#'
#' @param data data frame, which contains place chronology time pattern data.
#' @param id vector, which contains questionnaire ids. Use "all" if you want to plot all ids.
#' @param weekday vector, which contains days to plot.
#'
#' @return reshaped data frame for further visualization.
#' @export
get_places_chronology_time_pattern <- function(oc_data, id = "all", weekday = "all") {

  # Relevante Variablen auswählen
    data_pc_zm <-
    oc_data %>%
    ungroup() %>%
    select(
      questionnaire_id,
      day,
      duration,
      activity
    ) %>%
    mutate(
      activity = as.factor(activity)
    )

  # Nach ID filtern
  # Es darf dein NA Auschluss durchgeführt werden, weil sonst die Fahrzeit verloren geht!
  if (id[[1]] != "all") {
    data_pc_zm <- data_pc_zm %>% filter(questionnaire_id %in% id)
  }

  if (weekday[[1]] != "all") {
    data_pc_zm <- filter(data_pc_zm, day %in% weekday)
  }

  # Berechnung der Dauer für eine durchschnittliche Woche (nicht pro Woche)
  data_pc_zm <- data_pc_zm %>%
    group_by(questionnaire_id, day) %>%
    mutate(prop_duration = duration/sum(duration)) %>%
    ungroup() %>%
    group_by(questionnaire_id, day, activity) %>%
    mutate(prop_duration = sum(prop_duration)) %>%
    distinct(questionnaire_id, day, activity, prop_duration) %>%
    ungroup() %>%
    # Festlegen der Reihenfolge der Levels und orden der Daten.
    mutate(
        activity = fct_recode(
            activity,
            "Arbeit" = "Arbeitszeit",
            "Private Zeit" = "Freizeit",
        ),
          activity = fct_drop(fct_relevel(
            activity,
            "Lehrveranstaltung",
            "Lerngruppe",
            "Selbststudium",
            "Arbeit",
            "Fahrzeit",
            "Private Zeit",
            "Schlafen"
          ))
    ) %>%
    arrange(questionnaire_id, day, activity)

  data_pc_zm
}
