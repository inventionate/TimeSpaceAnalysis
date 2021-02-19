#' Reshpape place chronology time pattern data.
#'
#' @param data data frame, which contains place chronology time pattern data.
#' @param id vector, which contains questionnaire ids. Use "all" if you want to plot all ids.
#' @param weekday vector, which contains days to plot.
#'
#' @return reshaped data frame for further visualization.
#' @export
get_places_chronology_time_pattern <- function(oc_data, id = "all", weekday = "all") {

  print(oc_data)
  # Relevante Variablen auswählen
  oc_data <-
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

  print(oc_data)
  # Leeren Datensatz hinzufügen, um vergleichbare Ausgangsbedingungen zu schaffen.
  # D. h., jeder Person wird eine vergleichbare Aktivität mit der Länge Null hinzugefügt.
  # Für jede ID an jedem Tag eine Aktivität mit prop_duration 0 erzeugen.
  activities <- levels(oc_data$activity)

  data_scaffold <- oc_data %>%
    group_by(questionnaire_id, day) %>%
    distinct(.keep_all = TRUE) %>%
    na.omit()

  # @todo: Hier in Functional Programming switchen.
  data_zero_duration_activites <- NULL
  for(i in seq_along(activities))
  {
    data_zero_duration_activites <-
      bind_rows(
        data_zero_duration_activites,
        data_scaffold %>%
          mutate(
            duration = 0,
            activity = activities[i]
          )
      )
  }

  data_pc_zm <-
    oc_data <-
    bind_rows(
      oc_data,
      data_zero_duration_activites %>%
        mutate_at(vars(activity), ~ as.factor(.)))

  # Nach ID filtern
  # Es darf dein NA Auschluss durchgeführt werden, weil sonst die Fahrzeit verloren geht!
  if (id[[1]] != "all") {
    data_pc_zm <- oc_data %>% filter(questionnaire_id %in% id)
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
      activity = fct_relevel(
        activity,
        "Lehrveranstaltung",
        "Lerngruppe",
        "Selbststudium",
        "Fahrzeit",
        "Arbeitszeit",
        "Freizeit",
        "Schlafen"
      )
    ) %>%
    arrange(questionnaire_id, day, activity)

  data_pc_zm
}
