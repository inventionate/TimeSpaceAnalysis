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
#' @param axis_label_size size of y axis labels.
#' @param axis_cat_size size of x axis labels.
#' @param show_missing include missing values in plot or not (boolean).
#' @param open_sans use Open Sans font.
#'
#' @return ggplot2 barplot.
#' @export
plot_barplot <- function(df_origin,
                         df_var,
                         sort = FALSE,
                         bar_abs_size = 5.5,
                         bar_rel_size = 4.5,
                         axis_label_size = 12,
                         axis_cat_size = 16,
                         show_missing = TRUE,
                         digits = 1,
                         open_sans = TRUE){

  # Add Open Sans font family
  if (open_sans) .add_fonts()

  df_cat <-
    df_origin %>%
    select(var = !!sym(df_var)) %>%
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
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = abs),
      vjust = -1,
      nudge_y = 18,
      family = "Fira Sans",
      size = bar_abs_size
    ) +
    geom_text(
      aes(
        label = paste0("(", round(rel * 100, 1), " %)")
      ),
      vjust = -1,
      family = "Fira Sans",
      size = bar_rel_size
    ) +
    theme_void() +
    theme(
      text = element_text(family = "Fira Sans"),
      axis.text.x = element_text(size = axis_cat_size),
      axis.text.y = element_text(size = axis_label_size),
      axis.ticks = element_blank(),
      plot.caption = element_text(size = 10),
      legend.position = "none"
    ) +
    xlab("") +
    ylab("")
  # Add Tufte like marks
  tickmarks <-
    ggplot_build(p)$layout$panel_params[[1]]$y.labels[
      2:(length(ggplot_build(p)$layout$panel_params[[1]]$y.labels)-1)
      ] %>%
    as.numeric()
  p <-
    p +
    scale_y_continuous(
      limits = c(0, df_cat %>% pull(abs) %>% max() + 40),
      breaks = tickmarks,
    ) +
    scale_x_discrete(expand = expand_scale(add = 0.5)) +
    geom_hline(yintercept = tickmarks, col= "white", lwd = 1)

  p
}
