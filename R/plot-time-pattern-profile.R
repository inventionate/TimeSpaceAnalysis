 #' @include utilities.R
#' @include get-time-pattern-profile.R
NULL
#' Plot average time pattern profiles.
#'
#' @param data_tp data frame containing questionnaire_id, kml3d results and time pattern data.
#' @param id time pattern to plot.
#' @param ncol facet columns.
#' @param fluid should be static bars or fluid lines visualized (boolean).
#'
#' @return ggplot2 avgerage time pattern profile plot.
#' @export
plot_time_pattern_profile <- function(data_tp, id = "all", ncol = 2, fluid = FALSE, myriad = TRUE)
{
  # Add Myriad Pro font family
  if(myriad) .add_fonts()

  data_tsp <- get_time_pattern_profile(data_tp, id)

  # Fixe sieben Kategorien
  colours <- c("#f15b60", "#ce7058", "#faa75b", "#9e67ab", "#5a9bd4", "#7ac36a", "#737373")

  p <- ggplot(data_tsp, aes(x = day, y = prop_avg_duration))
  if(fluid) p <- p + geom_area(aes(fill = activity), position = "fill") + geom_vline(xintercept = c(1:7), linetype = "dotted", colour = "white")
  else p <- p + geom_bar(aes(fill = activity), position = "fill", stat = "identity", width = 1) + geom_vline(xintercept = c(1.5:6.5), linetype = "solid", colour = "white", size = 0.75)
  p <- p + scale_x_continuous(breaks = c(1:7), labels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"), name = "Wochentag") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"), name = "Zeitanteil in Prozent") +
    scale_fill_manual(name = "Tätigkeiten", values = colours)

  p <- add_theme(p) + coord_cartesian() + theme(legend.title = element_blank(), legend.position = "right")

  # Mehrere Gafiken parallel erzeugen
  p <- p + facet_wrap(~zeitmuster, ncol = ncol)

  return(p)
}
