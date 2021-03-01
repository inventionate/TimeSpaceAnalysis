#' @include utilities.R
NULL
#' Visualize a barplot.
#'
#' @param sort sort bars (boolean).
#' @param digits amount of label value digits.
#' @param df_origin source data farme (tibble).
#' @param df_var categorical variable name.
#' @param bar_abs_size size of absolute values in plot.
#' @param bar_rel_size size of relative values in plot.
#' @param show_missing include missing values in plot or not (boolean).
#' @param axes_rel_small relative value for small axes text (labels, titles …).
#'
#' @return ggplot2 barplot.
#' @export
plot_barplot <- function(df_origin, df_var, sort = FALSE, bar_abs_size = 3.5, bar_rel_size = 3, axes_rel_small = 0.6,
                         show_missing = TRUE, digits = 1){

  df_cat <-
    df_origin %>%
    select(var = {{ df_var }}) %>%
    mutate_all( fct_explicit_na, na_level = "fehlend" ) %>%
    count(var, name = "abs", sort = sort) %>%
    mutate(rel = abs / sum(abs) )

  if (!show_missing) {
    df_cat <-
      df_cat %>%
      filter(var != "fehlend")
  }

  df_cat_sum <-
    df_cat %>%
    summarise_at(2:3, sum) %>%
    add_column(var = df_var, .before = 1)

  p <-
    ggplot(df_cat, aes(var, abs)) +
    geom_bar(
      stat = "identity",
      width = .7
    ) +
    xlab("") +
    ylab("") +
    geom_text(
      aes(
        label = abs
      ),
      position = "stack",
      vjust = -2,
      family = "Fira Sans Condensed Semibold",
      size = bar_abs_size
    ) +
    geom_text(
      aes(
        label = paste0(round(rel * 100, 1), " %")
      ),
      position = "stack",
      vjust = -0.6,
      family = "Fira Sans Condensed Semibold",
      size = bar_rel_size
    )

  tickmarks <-
    ggplot_build(p)$layout$panel_params[[1]]$y.labels[
      1:(length(ggplot_build(p)$layout$panel_params[[1]]$y.labels))
      ] %>%
    as.numeric()

  p <-
    p +
    scale_y_continuous(
      limits = c(0, df_cat %>% pull(abs) %>% pretty() %>% max() * 1.1),
      breaks = tickmarks,
      expand = expansion(mult = c(0, 0.05)),
      labels = comma) +
    theme_minimal_hgrid(
      rel_small = 0.6,
      font_size = 12,
      font_family = "Fira Sans Condensed Semibold",
    ) +
    theme(aspect.ratio = 1)

  p
}
