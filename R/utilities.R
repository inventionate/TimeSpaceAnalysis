# Create plot
.create_plot <- function() {
  ggplot() +
    geom_hline(yintercept = 0, colour = "gray17") +
    geom_vline(xintercept = 0, colour = "gray17")
}

.finalize_plot <- function(plot,
                           res_gda,
                           axes = 1:2,
                           labels,
                           axis_label_y_vjust = 1,
                           axis_label_x_hjust = 1,
                           facet_labels = FALSE,
                           xlim = NULL,
                           ylim = NULL) {

  # Werte brechnen
  tickmarks_y <-
    ggplot_build(plot)$layout$panel_params[[1]]$y$breaks %>% keep(function(x) x %nin% c(NA, 0))

  tickmarks_x <-
    ggplot_build(plot)$layout$panel_params[[1]]$x$breaks %>% keep(function(x) x %nin% c(NA, 0))

  label_margin = 1.1

  if (!is_null(labels) && labels[2] != "") {
    label_margin = 2
  }

  # Plot aufbauen
  p <-
  plot +
  theme_void(
    base_size = 10,
    base_family = "Fira Sans Condensed"
  ) +
  theme(
    axis.title = element_text(
      face = "italic",
      colour = "gray17"
    ),
    axis.title.x = element_text(
      hjust = 1
    ),
    axis.title.y = element_text(
      hjust = 0
    ),
    legend.position = "none"
  )

  # Beschriftung ausführen!
  p <- .gda_plot_labels(
    res_gda = res_gda,
    ggplot_gda = p,
    axes = axes,
    mod_rates_message = FALSE
  )

  # Achsen und beschriftung neu ausrichten
  p <-
    p +
    theme(
      axis.text.x.top = element_blank(),
      axis.text.y.right = element_blank(),
      axis.title.x.top = element_text(
        margin = margin(0, 0, label_margin, 0, "cm"),
        hjust = axis_label_x_hjust
      ),
      axis.title.x.bottom = element_blank(),
      axis.title.y.right = element_text(
        margin = margin(0, 0, 0, 1.65, "cm"),
        angle = 0,
        vjust = axis_label_y_vjust
      ),
      axis.title.y.left = element_blank(),
      axis.text = element_text(
        size = 9,
        colour = "gray17"
      ),
      axis.ticks.length = unit(0.15, "cm"),
      axis.ticks = element_line(colour = "gray17")
    ) +
    scale_x_continuous(
      breaks = tickmarks_x,
      limits= xlim,
      sec.axis = dup_axis(),
      labels = scales::label_comma(decimal.mark = ",", big.mark = " ", accuracy = 0.1),
    ) +
    scale_y_continuous(
      breaks = tickmarks_y,
      limits = ylim,
      sec.axis = dup_axis(),
      labels = scales::label_comma(decimal.mark = ",", big.mark = " ", accuracy = 0.1)
    )

  g <- ggplotGrob(p)

  ax_b <- g[["grobs"]][g$layout$name == "axis-b"][[1]]

  ax_t <- g[["grobs"]][g$layout$name == "axis-t"][[1]]

  xlab_t <- g[["grobs"]][g$layout$name == "xlab-t"][[1]]

  ay_l <- g[["grobs"]][g$layout$name == "axis-l"][[1]]

  ay_r <- g[["grobs"]][g$layout$name == "axis-r"][[1]]

  ylab_r <- g[["grobs"]][g$layout$name == "ylab-r"][[1]]

  p <-
    p +
    annotation_custom(
      grid::grobTree(
        ax_b,
        vp = grid::viewport(
          y = 1,
          height = sum(ax_b$height)
        )
      ),
      ymax = 0,
      ymin = 0) +
    annotation_custom(
      grid::grobTree(
        ax_t,
        vp = grid::viewport(
          y = 1,
          height = sum(ax_t$height)
        )
      ),
      ymax = 0,
      ymin = 0) +
    annotation_custom(
      grid::grobTree(
        ay_l,
        vp = grid::viewport(
          x = 1,
          width = sum(ay_l$height)
        )
      ),
      xmax = 0,
      xmin = 0) +
    annotation_custom(
      grid::grobTree(
        ay_r,
        vp = grid::viewport(
          x = 1,
          width = sum(ay_r$height)
        )
      ),
      xmax = 0,
      xmin = 0) +
    annotation_custom(
      grid::grobTree(
        xlab_t,
        vp = grid::viewport(
          y = 1,
          height = sum(xlab_t$height)
        )
      ),
      ymax = 0,
      ymin = 0
    ) +
    annotation_custom(
      grid::grobTree(
        ylab_r,
        vp = grid::viewport(
          x = 1,
          height = sum(ylab_r$height)
        )
      ),
      xmax = 0,
      xmin = 0) +
    theme(
      axis.text = element_blank(),
      axis.title.x.top = element_blank(),
      axis.title.y.right = element_blank(),
      axis.ticks = element_blank()
    )

  if (!facet_labels) {
    p <- p + coord_fixed()
  }

  p
}

# Eine Sammlung hilfreicher Funktionen
.count_distinct_ind <- function(res_gda, axes = 1:2, normalize = NULL) {
  # Koordinaten der Individuen vorbereiten
  # Punkte an den gleichen Stellen vergößern und multiple entfernen.
  coord_ind <-
    tibble(x = res_gda$ind$coord[, axes[1]], y = res_gda$ind$coord[, axes[2]]) %>%
    group_by(x, y) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    data.frame()

  if (!is.null(normalize)) {
    coord_ind <-
      coord_ind %>%
      mutate_at("count", ~ ( . * (mean(normalize))^(1/2) ))
  }

  coord_ind
}

# Data frame selection
.select <- function(d, filter = NULL, check= TRUE) {

  if (!is.null(filter)) {

    # Filter by name
    if (!is.null(filter$name)) {
      name <- filter$name
      common <- intersect(name, d$name)
      diff <- setdiff(name, d$name)
      #if(check & length(common) == 0) stop("Can't find the specified names")
      # if(check & length(diff)!=0) warning("Can't find the the following name(s): ", diff)
      d <- d[common, , drop = FALSE]
    }

    # Filter by eta2
    if (!is.null(filter$eta2) & nrow(d) >= 1) {
      cos2 <- round(filter$eta2)
      d <- d[which(d$eta2 >= filter$eta2), , drop = FALSE]
    }

  }

  d
}

# Add Open Sans font
.add_fonts <- function() {
  showtext::showtext_auto()
  sysfonts::font_add(
    "Fira Sans Condensed",
    regular = "FiraSansCondensed-Regular.otf",
    italic = "FiraSansCondensed-Italic.otf",
    bold = "FiraSansCondensed-Bold.otf",
    bolditalic = "FiraSansCondensed-BoldItalic.otf"
  )
}

# Calculate crossed within variance
.crossed_within_variance <- function(var, weight, coord) {#, eigenvalues) {
  # Varianzen berechnen
  variances <-
    join(weight, coord, by = c("var1", "var2")) %>%
    group_by(!! var) %>%
    mutate(total_weight = sum(weight),
           relative_weight = weight/total_weight) %>%
    mutate_at(vars(matches("Dim")), ~ weighted.mean(., weight) - .) %>%
    summarise_at(vars(matches("Dim")), sum(relative_weight*(.^2)))# %>%
    #mutate_each(funs(. * eigenvalues$.), matches("Dim"))

  # Gesamte Anzahl an Personen
  weight_total <-
    weight %>%
    group_by(!! var) %>%
    summarise(weight_total = sum(weight))

  # Age within gender variance
  within_variance <-
    join(variances, weight_total, by = var) %>%
    summarise_at(vars(matches("Dim")), ~ weighted.mean(., weight_total))

  within_variance
}
# Extract eigenvalues
.get_eigenvalues <- function(res_gda) {

  eigenvalues <-
    res_gda$eig %>%
    data.frame %>%
    tibble::rownames_to_column() %>%
    separate(rowname, c("dim", "num")) %>%
    select(num, eigenvalue) %>%
    mutate_at(vars(matches("num")), as.numeric) %>%
    spread(num, eigenvalue)

  colnames(eigenvalues) <- paste0("Dim.", 1:ncol(eigenvalues))

  eigenvalues
}

# Select trajectory ind
.select_trajectory <- function(coord_all, select, time_point_names, axes) {

  # Vollständige Fälle bestimmen
  selected_ind_complete <-
    coord_all %>%
    count(id) %>%
    filter(n == length(time_point_names)) %>%
    select(id)

  coord_complete <-
    coord_all %>%
    filter(id %in% selected_ind_complete$id)

  # Info bzgl. der Anzahl kompleter Fälle.
  print(glue("Info: {length(selected_ind_complete$id)} complete cases."))

  # Selection (es wird select_ind definiert)
  selected_ind <- coord_all %>% select(id)
  # Durch Angabe des Namens filtern.
  if (!is.null(select$name)) {
    selected_ind <- selected_ind_name <- coord_all %>% filter(id %in% select$name)
  }

  # Durch die Un/Vollständigkeit der Fälle filtern
  if (!is.null(select$case)) {

    if (select$case == "complete") {
      selected_ind <- selected_ind_complete
    }

    if (select$case == "incomplete") {
      selected_ind <- selected_ind_incomplete <-
        coord_all %>%
        count(id) %>%
        filter(n != length(time_point_names)) %>%
        select(id)
    }

  }

  # Durch Angabe der Varianz filtern.
  if (!is.null(select$within_inertia)) {

    warning("Only complete cases will be used to calculate within inertia!")
    ind_mean_coord <-
      coord_complete %>%
      select(-time) %>%
      group_by(id) %>%
      summarise_all(~ mean)

    ind_mean_coord_id <-
      data.frame(ind_mean_coord)$id

    # "within inertia" berechnen (adaptiert von FactoMineR)
    tmp <- array(0, dim = c(dim(ind_mean_coord %>% select(-id)), length(time_point_names)))

    for (i in seq_along(time_point_names)) {
      tmp[,,i] <-
        as.matrix(
          coord_complete %>%
            filter(time == time_point_names[i] & id %in% ind_mean_coord_id) %>%
            select(-id, -time) - ind_mean_coord %>%
            select(-id)
        )^2 / length(time_point_names)
    }

    variab.auxil <- apply(tmp,2,sum)
    tmp <- sweep(tmp,2,variab.auxil,FUN="/") * 100
    inertie.intra.ind <- apply(tmp,c(1,2),sum)
    rownames(inertie.intra.ind) <- ind_mean_coord_id
    colnames(inertie.intra.ind) <- colnames(coord_complete %>% select(-id, -time))
    ind_within_inertia <- inertie.intra.ind

    if (select$within_inertia[[2]] == 0) {
      selected_ind_high <- NULL
    } else {
      selected_ind_high <-
        ind_within_inertia %>%
        data.frame %>%
        select(
          Dim.1 = matches(glue("Dim.{axes[1]}$")),
          Dim.2 = matches(glue("Dim.{axes[2]}$"))
        ) %>%
        tibble::rownames_to_column() %>%
        rename(id = rowname) %>%
        mutate(within_inertia = Dim.1 + Dim.2) %>%
        arrange(desc(within_inertia)) %>%
        slice(1:select$within_inertia[[2]])
    }

    if (select$within_inertia[[1]] == 0) {
      selected_ind_low <- NULL
    } else {
      selected_ind_low <-
        ind_within_inertia %>%
        data.frame %>%
        select(
          Dim.1 = matches(glue("Dim.{axes[1]}$")),
          Dim.2 = matches(glue("Dim.{axes[2]}$"))
        ) %>%
        tibble::rownames_to_column() %>%
        rename(id = rowname) %>%
        mutate(within_inertia = Dim.1 + Dim.2) %>%
        arrange(within_inertia) %>%
        slice(1:select$within_inertia[[1]])
    }
    selected_ind <- selected_ind_wi <- rbind(selected_ind_high, selected_ind_low)
  }

  if (!is.null(select$name) & !is.null(select$within_inertia)) {
    selected_ind <-
      selected_ind_name %>%
      filter(id %in% selected_ind_wi$id)
  }

  return(selected_ind)
}

# Gruppennamen der einzelnen Kategorien extrahieren
.get_group_names <- function(res_gda, group, group_names) {

  data <- res_gda$call$X %>% mutate_all(~sub("\\.", "_", .) )
  colnames(data) <- colnames(data) %>% sub("\\.", "_", .)

  if (is.null(res_gda$call$excl)) {
    var_num <- GDAtools::getindexcat(data)
  } else {
    var_num <- GDAtools::getindexcat(data)[-res_gda$call$excl]
  }

  var_num <- var_num %>%
    tibble(var.cat = .) %>% separate(var.cat, c("var", "cat"), sep = "[.]") %>%
    select(var) %>% count(var) %>% mutate_at(vars(var), as.factor)

  var <- tibble(var = colnames(data)) %>% mutate_all(as.factor)

  n_mod <- left_join(var, var_num, by = "var") %>% .$n
  # n_mod <- res_gda$call$X %>% mutate_all(n_distinct) %>% distinct

  n_mod_group <- NULL
  start <- 0
  for (i in seq_along(group)) {
    n_mod_group <- c(n_mod_group, sum( n_mod[(start + 1):(start + group[i])] ) )
    start <- sum( group[1:i] )
  }

  # Gruppenzuordnung der Modalitäten
  df_group_names <- data.frame(group = rep(group_names, n_mod_group))

  return(df_group_names)
}
# Beschriftung eines Plots anpassen
.gda_plot_labels <- function(res_gda,
                             ggplot_gda,
                             title = waiver(),
                             axes = 1:2,
                             plot_modif_rates = TRUE,
                             eta2 = NULL,
                             axis_lab_name = "Achse",
                             mod_rates_message = TRUE) {

  caption = waiver()

  if (plot_modif_rates) {

    modif_rates <- modified_rates(res_gda)

    rate_1 <- format(modif_rates[[axes[1], 1]], decimal.mark = ",")

    rate_2 <- format(modif_rates[[axes[2], 1]], decimal.mark = ",")

    if (mod_rates_message) {

      caption = "Reskalierte Eigenwerte nach Benzécri"

    }


  } else {

    eig <- factoextra::get_eigenvalue(res_gda)[,2]

    rate_1 <- format(round(eig[[axes[1]]], 1), decimal.mark = ",")

    rate_2 <- format(round(eig[[axes[2]]], 1), decimal.mark = ",")

  }

  if (!is.null(eta2)) {

    xlab = str_glue("{axis_lab_name} {axes[1]}\n({rate_1} %;
                    η² = {format(eta2[axes[1]], decimal.mark = ',')})")

    ylab = str_glue("{axis_lab_name} {axes[2]}\n({rate_2} %;
                    η² = {format(eta2[axes[2]], decimal.mark = ',')})")

  } else {

    xlab = str_glue("{axis_lab_name} {axes[1]}\n({rate_1} %)")

    ylab = str_glue("{axis_lab_name} {axes[2]}\n({rate_2} %)")

  }

  p <- ggplot_gda + labs(title = title, x = xlab, y = ylab, caption = caption)

  p
}

# Achsen beschriften
.annotate_axes <- function(plot, labels = NULL, alpha = 0.7) {

  if (is_null(labels)) return(plot)

    if (labels[1] != "") {
      plot <-
        plot +
        annotate(
          "label",
          x = -Inf,
          y = 0.008,
          size = 4,
          hjust = 0,
          vjust = 0,
          label.padding = unit(0.2, "lines"),
          label.r = unit(0, "lines"),
          label.size = 0,
          fill = "gray80",
          alpha = alpha,
          label = toupper(labels[1]),
          family = "Fira Sans Condensed",
          fontface = "bold"
        )
    }
    if (labels[2] != "") {
      plot <-
        plot +
        annotate(
          "label",
          x = Inf,
          y = 0.008,
          size = 4,
          hjust = 1,
          vjust = 0,
          label.padding = unit(0.2, "lines"),
          label.r = unit(0, "lines"),
          label.size = 0,
          fill = "gray80",
          alpha = alpha,
          label = toupper(labels[2]),
          family = "Fira Sans Condensed",
          fontface = "bold"
        )
    }
    if (labels[3] != "") {
      plot <-
        plot +
        annotate(
          "label",
          x = -0.008,
          y = Inf,
          size = 4,
          hjust = 1,
          vjust = 1,
          label.padding = unit(0.2, "lines"),
          label.r = unit(0, "lines"),
          label.size = 0,
          fill = "gray80",
          alpha = alpha,
          label = toupper(labels[3]),
          family = "Fira Sans Condensed",
          fontface = "bold"
        )
    }
      if (labels[4] != "") {
        plot <-
          plot +
          annotate(
            "label",
            x = -0.008,
            y = -Inf,
            size = 4,
            hjust = 1,
            vjust = 0,
            label.padding = unit(0.2, "lines"),
            label.r = unit(0, "lines"),
            label.size = 0,
            fill = "gray80",
            alpha = alpha,
            label = toupper(labels[4]),
            family = "Fira Sans Condensed",
            fontface = "bold"
          )
      }

  plot
}
