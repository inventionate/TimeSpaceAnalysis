#' @include utilities.R
#' @include get-index-mod.R
#' @include get-mfa-mod-group-id.R
NULL
#' Visualize specific contributing modalities in a plane.
#'
#' @param res_gda GDA result data frame.
#' @param contrib "auto" calculates the optimal modalities to show (based on the basic criterion). Otherwise define an amount of modalities to plot.
#' @param title plot title.
#' @param axes the GDA dimensions to plot.
#' @param open_sans use Open Sans font (boolean).
#' @param group vector containing group definition.
#' @param group_names names of the groups.
#' @param group_style style to plot (vector containing "shape", "colour" or "both).
#' @param textsize size of the text.
#' @param colour_palette name of the used colour palette.
#' @param individuals show individual points/ biplot (boolean).
#' @param plot_modif_rates plot modified rates instead of eigenvalue percentage (boolean).
#' @param individuals_size set individual point size manual or "auto".
#' @param individuals_alpha set alpha value.
#' @param individuals_names plot individual names (boolean).
#' @param axis_lab_name name of axis label.
#' @param group_lab_name name of variable groups.
#' @param labels label axes (vector of length 4; left, right, top, bottom).
#'
#' @return ggplot2 visualization containing selected modalities.
#' @export
fviz_gda_var <- function(res_gda,
                         contrib = "auto",
                         title = NULL,
                         axes = 1:2,
                         open_sans = TRUE,
                         group = NULL,
                         group_names = NULL,
                         group_style = "both",
                         textsize = 4,
                         colour_palette = "Set1",
                         individuals = FALSE,
                         individuals_size = "auto",
                         individuals_alpha = 0.5,
                         individuals_names = FALSE,
                         plot_modif_rates = TRUE,
                         axis_lab_name = "Achse",
                         group_lab_name = "Themengruppen",
                         labels = NULL ) {
  # Check GDA algorithm
  if (inherits(res_gda, c("MCA"))) {
    df <- res_gda$var$contrib
  } else {
    stop("Only MCA plots are currently supported!")
  }

  # Add Open Sans font family
  if (open_sans) .add_fonts()

  # Evaluate axes
  axis_1 <- sym(paste0("Dim.", axes[1]))
  axis_2 <- sym(paste0("Dim.", axes[2]))

  # Eigenwerte extrahieren
  eigenvalues <- .get_eigenvalues(res_gda)

  # Calculate contribution criterion (Le Roux & Rouanet 2004: 372)
  if (is.null(res_gda$call$excl)) {
    criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)))
  } else {
    criterion <- 100/(length(GDAtools::getindexcat(res_gda$call$X)[-res_gda$call$excl]))
  }

  # Auswahl festlegen
  modalities <-
    df %>%
    data.frame() %>%
    select(matches(paste0("^Dim.", axes[1], "|", axes[2], "$"))) %>%
    tibble::rownames_to_column(var = "category") %>%
    gather(dim, ctr, -category) %>%
    arrange(desc(ctr))

  if (contrib == "auto") {
    modalities <-
      modalities %>%
      filter(ctr > criterion) %>%
      select(category) %>%
      data.frame()
  } else {

    modalities <-
      df %>%
      data.frame() %>%
      select(matches(str_glue("^Dim.{axes[1]}|{axes[2]}$"))) %>%
      tibble::rownames_to_column(var = "category") %>%
      mutate_at(vars(matches("Dim")), function(x) {x * eigenvalues$.}) %>%
      mutate(ctr = !!parse_quo(str_glue("Dim.{axes[1]} + Dim.{axes[2]}"))) %>%
      arrange(desc(ctr)) %>%
      slice(1:contrib) %>%
      select(category) %>%
      data.frame()

  }

  if (inherits(res_gda, c("MCA"))) {

    if (!is.null(group)) {

      # Gruppennamen festlegen, falls keine angegeben
      if (is.null(group_names)) {
        group_names <- paste0("Group_", seq_along(group))
      }

      # Checken, on Definition korrekt war
      if (length(group) != length(group_names)) {
        stop("Wrongt group and group name definition!")
      }

      # Anzahl der Kategorien zählen
      df_group_names <- .get_group_names(res_gda, group, group_names)

      # Zeilennamen hinzufügen
      if (is.null(res_gda$call$excl)) {
        col_weight <- res_gda$call$marge.col
      } else {
        col_weight <- res_gda$call$marge.col[-res_gda$call$excl]
      }

      modalities_coord <-
        res_gda$var$coord %>%
        data.frame() %>%
        tibble::rownames_to_column() %>%
        bind_cols(
          .,
          df_group_names,
          data.frame(weight = col_weight * res_gda$call$N)
        ) %>%
        filter(rowname %in% modalities$category)

      # Plot
      p <-
        fviz_mca_var(
          res_gda,
          label = "none",
          select.var = list(name = modalities$category),
          axes.linetype = "blank",
          axes = axes,
          pointsize = 0
        ) +
        geom_hline(
          yintercept = 0,
          colour = "gray17",
          linetype = "solid"
        ) +
        geom_vline(
          xintercept = 0,
          colour = "gray17",
          linetype = "solid"
        )

      if (individuals) {
        if (individuals_size == "auto") {
          p <-
            p +
            geom_point(
              data = .count_distinct_ind(res_gda, axes, modalities_coord$weight) %>%
                distinct(),
              aes(x, y, size = count),
              inherit.aes = FALSE,
              alpha = individuals_alpha
            )
        } else {
          p <-
            p +
            geom_point(
              data = .count_distinct_ind(res_gda, axes) %>% distinct(),
              aes(x, y),
              size = individuals_size,
              inherit.aes = FALSE,
              alpha = individuals_alpha
            )
        }
      }

      if (individuals_names) {
        p <-
          p +
          ggrepel::geom_label_repel(
            data = .count_distinct_ind(res_gda, axes),
            aes(
              !!axis_1,
              !!axis_2,
              label = rownames(.count_distinct_ind(res_gda, axes))
            ),
            colour = "black",
            size = individuals_size,
            alpha = individuals_alpha
          )
      }

      if (group_style == "both") {
        p <-
          p +
          geom_point(
            data = modalities_coord,
            aes(
              !!axis_1,
              !!axis_2,
              colour = group,
              shape = group,
              size = weight
            )
          )
      }

      if (group_style == "colour") {
        p <-
          p +
          geom_point(
            data = modalities_coord,
            aes(
              !!axis_1,
              !!axis_2,
              colour = group,
              size = weight
            ),
            shape = 17
          )
      }

      if (group_style == "shape") {
        p <-
          p +
          geom_point(
            data = modalities_coord,
            aes(
              !!axis_1,
              !!axis_2,
              shape = group,
              size = weight
            ),
          colour = "black"
        )
      }

      if (group_style %in% c("colour", "both")) {
        p <-
          p +
          ggrepel::geom_text_repel(
            data = modalities_coord,
            aes(
              !!axis_1,
              !!axis_2,
              colour = group,
              label = factor(modalities_coord$rowname)
            ),
            size = textsize,
            show.legend = FALSE
          )
      }

      if (group_style == "shape") {
        p <-
          p +
          ggrepel::geom_text_repel(
            data = modalities_coord,
            aes(
              !!axis_1,
              !!axis_2,
              label = factor(modalities_coord$rowname)
            ),
            size = textsize,
            show.legend = FALSE
          )
      }

    } else {
      # Plot group specific modalities
      p <-
        fviz_mca_var(
          res_gda,
          col.var = "black",
          repel = TRUE,
          select.var = list(name = modalities$category),
          axes.linetype = "blank",
          axes = axes
        ) +
        geom_hline(
          yintercept = 0,
          colour = "gray17",
        ) +
        geom_vline(
          xintercept = 0,
          colour = "gray17",
        )

    }

  }

  if (!is_null(title)) p <- p + ggtitle(title)

  # Legende für Größen ausblenden
  p <- p + scale_size(guide = FALSE)

  p <- .finalize_plot(p, res_gda, axes, labels)

  if (!is.null(group_style) & !is.null(group)) {

    if (group_style %in% c("colour", "both")) {
      p <-
        p +
        scale_colour_brewer(
          name = str_glue("{group_lab_name}"),
          palette = colour_palette,
          labels = modalities_coord %>%
            select(group) %>%
            distinct(),
          type = "qualitative"
        )
    }

    if (group_style %in% c("shape", "both")) {
      p <-
        p +
        scale_shape_discrete(
          name = str_glue("{group_lab_name}"),
          labels = modalities_coord %>%
            select(group) %>%
            distinct(),
          solid = TRUE
        )
    }

    p <-
      p +
      theme(
        plot.title = element_blank(),
        # @TODO: Make position adjustable
        legend.position = c(0.12, 0.1),
        legend.box.background = element_rect(
          linetype = "solid",
          colour = "gray17",
          fill = "white"
        ),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(0, 0.2, 0.1, 0, "cm"),
        legend.title = element_blank()
      ) +
      guides(
        colour = guide_legend(
          override.aes = list(size = 2.5)
        )
      )
  }

  #if(individuals_size == "auto") p <- p + scale_size_continuous(range = c(3, 3 * max(.count_distinct_ind(res_gda)$count)), guide = FALSE)

  # Beschriftung anpassen
  p <- .annotate_axes(p, labels)

  # Plotten
  p
}
