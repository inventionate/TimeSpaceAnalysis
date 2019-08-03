#' @include utilities.R
NULL
#' Visualize a barplot.
#'
#' @param dfname categorical variable.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param sort sort bars (boolean).
#' @param labels_inline inline labels (boolean).
#' @param amount include total amount of observations (boolean).
#' @param rotate_x_axis_text rotate bars (boolean).
#' @param title plot title.
#' @param textsize sife of axes texts.
#' @param titlesize title text size.
#' @param labelsize label text size.
#' @param digits amount of label value digits.
#' @param ylim y-axis range.
#' @param xlim y-axis range.
#' @param include_na show NAs or not (boolean).
#' @param open_sans use Open Sans font.
#' @param abs_freq use absolute or relative freq (boolean).
#' @param symbol relative freq symbol.
#'
#' @return ggplot2 barplot.
#' @export
plot_barplot <- function(dfname,
                         xlab = "",
                         ylab = "",
                         title = "",
                         sort = FALSE,
                         labels_inline = FALSE,
                         amount = FALSE,
                         rotate_x_axis_text = FALSE,
                         textsize = 20,
                         titlesize = 25,
                         labelsize = 8,
                         include_na = TRUE,
                         digits = 0,
                         ylim = NA,
                         xlim = NA,
                         open_sans = TRUE,
                         abs_freq = TRUE,
                         symbol = "%"){

  # Add Open Sans font family
  if (open_sans) .add_fonts()

  if (include_na) {
    absolute_freq <- table(dfname, useNA = "always")
  } else {
    absolute_freq <- table(dfname)
  }

  if (sort) absolute_freq <- as.table(sort(absolute_freq))

  relative_freq <- as.data.frame(round(prop.table(absolute_freq) * 100, digits = digits))

  data_freq <- data.frame(as.data.frame(absolute_freq), relative_freq[,2])

  colnames(data_freq) <- c("var", "absolute", "relative")

  # Choose absolute or relative.
  if (abs_freq) {

    freq <-  "absolute"
    n <- nrow(data.frame(na.omit(dfname)))

  } else {

    freq <-  "relative"
    n <- 100

  }

  p <-
    # @CHECK is freq works.
    ggplot(data_freq, aes(var, !! freq)) +
    geom_bar(stat="identity") +
    xlab(xlab) +
    ylab(ylab) +
    ylim(0, n) +
    ggtitle(title)

  # @TODO replace add_theme here, This function must only be used by GDA functions.
  p <-
    add_theme(p) +
    coord_cartesian() +
    theme(
      panel.border = element_blank(),
      axis.text = element_text(size = 12),
      axis.line = element_line(colour = "gray70", size = 0.75)
    )

  if (!is.na(xlim[1])) p <- p + xlim(xlim)

  if (!is.na(ylim[1])) p <- p + ylim(ylim)

  if (labels_inline) {

    if (abs_freq) {

      p <-
        p +
        geom_text(
          aes(label = absolute),
          family = "Fira Sans",
          vjust = 1.5,
          size = labelsize,
          colour = "white",
          position = "stack"
        ) +
        geom_text(
          aes(label = str_glue("({relative}{symbol})")),
          family = "Fira Sans",
          vjust = 4,
          size = labelsize/1.5,
          colour = "white",
          position = "stack"
        )

    } else {

      p <-
        p +
        geom_text(
          aes(label = str_glue("{relative}{symbol}")),
          family = "Fira Sans",
          vjust = 1.5,
          size = labelsize,
          colour = "white",
          position = "stack"
        ) +
        geom_text(
          aes(label = str_glue("({absolute})")),
          family = "Fira Sans",
          vjust = 4,
          size = labelsize/1.5,
          colour = "white",
          position = "stack"
        )

    }

  } else {

    if (abs_freq) {

      p <-
        p +
        geom_text(
          aes(label = absolute),
          family = "Fira Sans",
          vjust = -1.5,
          size = labelsize,
          colour = "black",
          position = "stack"
        ) +
        geom_text(
          aes(label = str_glue("({relative}{symbol})")),
          family = "Fira Sans",
          vjust = -0.5,
          size = labelsize/1.5,
          colour = "black",
          position = "stack"
        )

    } else {

      p <-
        p +
        geom_text(
          aes(label = str_glue("{relative}{symbol}")),
          family = "Fira Sans",
          vjust = -1.5,
          size = labelsize,
          colour = "black",
          position = "stack"
        ) +
        geom_text(
          aes(label = str_glue("({absolute})")),
          family = "Fira Sans",
          vjust = -0.5,
          size = labelsize/1.5,
          colour = "black",
          position = "stack"
        )

    }

  }

  if (rotate_x_axis_text) {
    p <-
      p +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  if (amount) {
    p <-
      p +
      xlab(str_glue("{xlab} (n = {nrow(data.frame(dfname))})")) +
      theme(axis.title.x = element_text(vjust = -0.3))
  }

  p
}
