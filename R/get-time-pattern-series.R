#' Reshape time pattern series data.
#'
#' @param data_tp data frame including questionnaire_id, kml3d results and time pattern data.
#'
#' @return Reshaped data frame.
#' @export
get_time_pattern_series <- function(data_tp) {

  # Daten für die Zeitserien aufbereiten
  data_series <-
    data_tp %>%
    gather(
      "day_activity",
      "duration",
      3:51) %>%
    mutate(
      day_activity = str_replace(day_activity, "_\\d+", "")
    ) %>%
    separate(
      day_activity,
      c("day", "activity"),
      "_"
    ) %>%
    mutate(
      day = fct_relevel(
        str_to_title(day),
        "Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"
      ),
      activity = fct_recode(
        str_to_title(activity),
        "Lehrveranstaltungen" = "Veranstaltungen",
        "Private Zeit" = "Freizeit",
        "Arbeit" = "Arbeitszeit")
    ) %>%
    mutate(
      activity = fct_relevel(
        activity,
        "Lehrveranstaltungen",
        "Zwischenzeit",
        "Selbststudium",
        "Fahrzeit",
        "Arbeit",
        "Private Zeit",
        "Schlafen")
    ) %>%
    drop_na()

  # Durchschnittsprofile berechnen
  data_series_average <-
    data_series %>%
    select(-questionnaire_id) %>%
    drop_na() %>%
    group_by(zeitmuster, day, activity) %>%
    summarise(avg_duration = mean(duration)) %>%
    ungroup()

  # Prozentuale Verteilung der Zeitmuster
  data_series_profile_prop <-
    data_tp |>
    select(zeitmuster) |>
    count(zeitmuster) |>
    transmute(
      zeitmuster = fct_explicit_na(zeitmuster, "Fehlend"),
      prop = str_glue("{format(round(n / sum(n) * 100, 1), decimal.mark=',')} %"),
      n = n
    )

  list(
    data_series = data_series,
    data_series_average = data_series_average,
    data_series_profile_prop_label = data_series_profile_prop
  )
}
