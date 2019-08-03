#' Reshape time pattern data.
#'
#' @param data data frame which contains time pattern data.
#' @param id vector which contains questionnaire ids.
#' @param reshape_data whether reshape data or not.
#' Use this option if your data is column wise concentration (e. g. "mo_seminar")
#'
#' @return reshaped data frame for further visualization.
#' @export
get_time_pattern <- function(data,
                             id = "all",
                             reshape_data = TRUE) {

  # Check NA
  na_exist <- nrow(data) > nrow(na.omit(data))
  if( na_exist ) warning("There are NAs. They will be omitted!")

  # Filter ID
  if (id[[1]] != "all") {
    data_tp <- filter(na.omit(data), questionnaire_id %in% id)
  } else {
    data_tp <- na.omit(data)
  }

  if (reshape_data) {
    data_time_pattern <- data_tp %>%
      gather(
        activity,
        duration,
        3:ncol(data_tp),
        -questionnaire_id
      ) %>%
      mutate(
        activity = str_replace(activity, "_\\d+", "")
      ) %>%
      separate(
        activity,
        c("day", "activity"),
        sep ="_"
      ) %>%
      mutate(
        day = case_when(
          day == "mo" ~ 1,
          day == "di" ~ 2,
          day == "mi" ~ 3,
          day == "do" ~ 4,
          day == "fr" ~ 5,
          day == "sa" ~ 6,
          day == "so" ~ 7
        )
      ) %>%
      mutate(
        activity = fct_recode(
          str_to_title(activity),
          "Lehrveranstaltungen" = "Veranstaltungen"
        )
      ) %>%
      mutate(
        activity = fct_relevel(
          activity,
          "Lehrveranstaltungen",
          "Zwischenzeit",
          "Selbststudium",
          "Arbeitszeit",
          "Fahrzeit",
          "Freizeit",
          "Schlafen"
        )
      ) %>%
      group_by(questionnaire_id, day) %>%
      mutate(prop_duration = duration / sum(duration)) %>%
      arrange(questionnaire_id, day, desc(activity)) %>%
      ungroup()
  } else {
    data_time_pattern <- data_tp %>%
      ungroup() %>%
      mutate(
        day = case_when(
          day == "Montag" ~ 1,
          day == "Dienstag" ~ 2,
          day == "Mittwoch" ~ 3,
          day == "Donnerstag" ~ 4,
          day == "Freitag" ~ 5,
          day == "Samstag" ~ 6,
          day == "Sonntag" ~ 7
        )
      )
  }

  data_time_pattern
}
