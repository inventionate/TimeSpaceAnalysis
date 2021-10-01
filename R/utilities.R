# Create plot
.create_plot <- function() {
  ggplot() +
    geom_hline(yintercept = 0, colour = "gray17") +
    geom_vline(xintercept = 0, colour = "gray17")
}

.finalize_plot <- function(plot,
                           res_gda,
                           axes = 1:2,
                           labels = NULL,
                           axis_label_y_vjust = 1,
                           axis_label_x_hjust = 1,
                           facet_labels = FALSE,
                           xlim = NULL,
                           ylim = NULL,
                           accuracy = 0.1) {

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
    base_family = "Fira Sans Condensed Medium"
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
      plot.title = element_textbox(family= "Fira Sans Condensed Medium", face = "bold", size = 15),
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
      labels = scales::label_comma(decimal.mark = ",", big.mark = " ", accuracy = accuracy)
    ) +
    scale_y_continuous(
      breaks = tickmarks_y,
      limits = ylim,
      sec.axis = dup_axis(),
      labels = scales::label_comma(decimal.mark = ",", big.mark = " ", accuracy = accuracy)
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
        text = element_text(family = "Fira Sans Condensed"),
        axis.text = element_blank(),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        axis.ticks = element_blank()
    )

  if (!facet_labels) {
    p <- p + coord_fixed()
  }

  p <- gginnards::move_layers(p, "GeomCustomAnn" ,position = "bottom")

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

# Calculate crossed within variance
.crossed_within_variance <- function(var, weight, coord) {#, eigenvalues) {
  # Varianzen berechnen
  variances <-
    join(weight, coord, by = c("var1", "var2")) %>%
    group_by({{ var }}) %>%
    mutate(total_weight = sum(weight),
           relative_weight = weight/total_weight) %>%
    mutate_at(vars(matches("Dim")), ~ weighted.mean(., weight) - .) %>%
    summarise_at(vars(matches("Dim")), sum(relative_weight*(.^2)))# %>%
    #mutate_each(funs(. * eigenvalues$.), matches("Dim"))

  # Gesamte Anzahl an Personen
  weight_total <-
    weight %>%
    group_by({{ var }}) %>%
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

    warning("Only complete cases will be used to calculate within inertia!", call. = FALSE)
    ind_mean_coord <-
      coord_complete %>%
      select(-time) %>%
      group_by(id) %>%
      summarise_all(~ mean(.))

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
.annotate_axes <- function(plot, labels = NULL, alpha = 0.7, pos_adjust = 0.008) {

  if (is_null(labels)) return(plot)

    if (labels[1] != "") {
      plot <-
        plot +
        annotate(
          "label",
          x = -Inf,
          y = pos_adjust,
          size = 4,
          hjust = 0,
          vjust = 0,
          label.padding = unit(0.2, "lines"),
          label.r = unit(0, "lines"),
          label.size = 0,
          fill = "gray80",
          alpha = alpha,
          label = str_to_upper(labels[1], "de"),
          family = "Fira Sans Condensed Medium",
          fontface = "bold"
        )
    }
    if (labels[2] != "") {
      plot <-
        plot +
        annotate(
          "label",
          x = Inf,
          y = pos_adjust,
          size = 4,
          hjust = 1,
          vjust = 0,
          label.padding = unit(0.2, "lines"),
          label.r = unit(0, "lines"),
          label.size = 0,
          fill = "gray80",
          alpha = alpha,
          label = str_to_upper(labels[2], "de"),
          family = "Fira Sans Condensed Medium",
          fontface = "bold"
        )
    }
    if (labels[3] != "") {
      plot <-
        plot +
        annotate(
          "label",
          x = -pos_adjust,
          y = Inf,
          size = 4,
          hjust = 1,
          vjust = 1,
          label.padding = unit(0.2, "lines"),
          label.r = unit(0, "lines"),
          label.size = 0,
          fill = "gray80",
          alpha = alpha,
          label = str_to_upper(labels[3], "de"),
          family = "Fira Sans Condensed Medium",
          fontface = "bold"
        )
    }
      if (labels[4] != "") {
        plot <-
          plot +
          annotate(
            "label",
            x = -pos_adjust,
            y = -Inf,
            size = 4,
            hjust = 1,
            vjust = 0,
            label.padding = unit(0.2, "lines"),
            label.r = unit(0, "lines"),
            label.size = 0,
            fill = "gray80",
            alpha = alpha,
            label = str_to_upper(labels[4], "de"),
            family = "Fira Sans Condensed Medium",
            fontface = "bold"
          )
      }

  plot
}

# scalebar
# From https://github.com/oswaldosantos/ggsn/blob/master/R/scalebar.R
scalebar <- function(data = NULL,
                     location = "bottomright",
                     dist = NULL,
                     dist_unit = NULL,
                     transform = NULL,
                     dd2km = NULL,
                     model = NULL,
                     height = 0.02,
                     st.dist = 0.02,
                     st.bottom = TRUE,
                     st.size = 5,
                     st.color = "black",
                     box.fill = c("black", "white"),
                     box.color = "black",
                     border.size = 1,
                     x.min = NULL,
                     x.max = NULL,
                     y.min = NULL,
                     y.max = NULL,
                     anchor = NULL,
                     facet.var = NULL,
                     facet.lev = NULL,
                     st.inherit = TRUE,
                     unit_pos_dist = 0.5) {
  if (is.null(data)) {
    if (is.null(x.min) | is.null(x.max) |
      is.null(y.min) | is.null(y.max)) {
      stop("If data is not defined, x.min, x.max, y.min and y.max must be.")
    }
    data <- data.frame(long = c(x.min, x.max), lat = c(y.min, y.max))
  }
  if (is.null(transform)) {
    stop("transform should be logical.")
  }
  if (any(class(data) %in% "sf")) {
    xmin <- sf::st_bbox(data)["xmin"]
    xmax <- sf::st_bbox(data)["xmax"]
    ymin <- sf::st_bbox(data)["ymin"]
    ymax <- sf::st_bbox(data)["ymax"]
  } else {
    if (any(startsWith(colnames(data), "lat")) & any(startsWith(colnames(data), "long"))) {
      xmin <- min(data$long)
      xmax <- max(data$long)
      ymin <- min(data$lat)
      ymax <- max(data$lat)
    } else {
      stop("'", substitute(data), "' must have columns with names that start with 'lat' and 'long'")
    }
  }
  if (location == "bottomleft") {
    if (is.null(anchor)) {
      x <- xmin
      y <- ymin
    } else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- 1
  }
  if (location == "bottomright") {
    if (is.null(anchor)) {
      x <- xmax
      y <- ymin
    } else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- -1
  }
  if (location == "topleft") {
    if (is.null(anchor)) {
      x <- xmin
      y <- ymax
    } else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- 1
  }
  if (location == "topright") {
    if (is.null(anchor)) {
      x <- xmax
      y <- ymax
    } else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- -1
  }
  if (!st.bottom) {
    st.dist <-
      y + (ymax - ymin) * (height + st.dist)
  } else {
    st.dist <- y - (ymax - ymin) * st.dist
  }
  height <- y + (ymax - ymin) * height

  if (dist_unit == "m") {
    dist <- dist / 1e3
    dist_unit0 <- "m"
    dist_unit <- "km"
  }
  if (!is.null(dd2km)) {
    if (dd2km) {
      transform <- TRUE
    }
    cat("dd2km is deprecated. Use ggsn::transform instead.")
  }
  if (transform) {
    break1 <- maptools::gcDestination(
      lon = x,
      lat = y,
      bearing = 90 * direction,
      dist = dist,
      dist.units = dist_unit,
      model = model
    )[1, 1]
    break2 <- maptools::gcDestination(
      lon = x,
      lat = y,
      bearing = 90 * direction,
      dist = dist * 2,
      dist.units = dist_unit,
      model = model
    )[1, 1]
  } else {
    if (location == "bottomleft" | location == "topleft") {
      if (exists("dist_unit0") | (!exists("dist_unit0") & dist_unit == "km")) {
        break1 <- x + dist * 1e3
        break2 <- x + dist * 2e3
      } else if (dist_unit == "nm") {
        break1 <- x + dist * 1852
        break2 <- x + dist * 1852 * 2
      } else if (dist_unit == "mi") {
        break1 <- x + dist * 1609.34
        break2 <- x + dist * 1609.34 * 2
      } else {
        break1 <- x + dist
        break2 <- x + dist
      }
    } else {
      if (exists("dist_unit0") | (!exists("dist_unit0") & dist_unit == "km")) {
        break1 <- x - dist * 1e3
        break2 <- x - dist * 2e3
      } else if (dist_unit == "nm") {
        break1 <- x - dist * 1852
        break2 <- x - dist * 1852 * 2
      } else if (dist_unit == "mi") {
        break1 <- x - dist * 1609.34
        break2 <- x - dist * 1609.34 * 2
      } else {
        break1 <- x - dist
        break2 <- x - dist
      }
    }
  }

  out_of_range <- function(low, n, high) {
    n < low | n > high
  }

  if (out_of_range(xmin, break1, xmax) | out_of_range(xmin, break2, xmax)) {
    stop(
      "The requested scalebar distance (",
      substitute(dist), " ", substitute(dist_unit),
      ") is too large to fit on the map.\n  Try reducing it."
    )
  }


  box1 <- data.frame(
    x = c(x, x, rep(break1, 2), x),
    y = c(y, height, height, y, y),
    group = 1
  )
  box2 <- data.frame(
    x = c(rep(break1, 2), rep(break2, 2), break1),
    y = c(y, rep(height, 2), y, y),
    group = 1
  )
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    for (i in 1:length(facet.var)) {
      if (any(class(data) == "sf")) {
        if (!is.factor(data[, facet.var[i]][[1]])) {
          data[, facet.var[i]] <- factor(data[, facet.var[i]][[1]])
        }
        box1[, facet.var[i]] <- factor(
          facet.lev[i],
          levels(data[, facet.var[i]][[1]])
        )
        box2[, facet.var[i]] <- factor(
          facet.lev[i],
          levels(data[, facet.var[i]][[1]])
        )
      } else {
        if (!is.factor(data[, facet.var[i]])) {
          data[, facet.var[i]] <- factor(data[, facet.var[i]])
        }
        box1[, facet.var[i]] <- factor(
          facet.lev[i],
          levels(data[, facet.var[i]])
        )
        box2[, facet.var[i]] <- factor(
          facet.lev[i],
          levels(data[, facet.var[i]])
        )
      }
    }
  }
  if (exists("dist_unit0")) {
    legend <- cbind(text = c(0, dist * 1e3, dist * 2e3), row.names = NULL)
  } else {
    legend <- cbind(text = c(0, dist, dist * 2), row.names = NULL)
  }
  gg.box1 <- geom_polygon(
    data = box1, aes(x, y),
    fill = utils::tail(box.fill, 1),
    color = utils::tail(box.color, 1),
    size = border.size
  )
  gg.box2 <- geom_polygon(
    data = box2, aes(x, y), fill = box.fill[1],
    color = box.color[1],
    size = border.size
  )
  x.st.pos <- c(box1[c(1, 3), 1], box2[3, 1])
  if (location == "bottomright" | location == "topright") {
    x.st.pos <- rev(x.st.pos)
  }
  label <- NULL
  if (exists("dist_unit0")) {
    legend2 <- cbind(data[1:3, ],
      x = unname(x.st.pos),
      y = unname(st.dist),
      label = legend[, "text"]
    )
  } else {
    legend2 <- cbind(data[1:3, ],
      x = unname(x.st.pos),
      y = unname(st.dist),
      label = legend[, "text"]
    )
  }
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    for (i in 1:length(facet.var)) {
      if (any(class(data) == "sf")) {
        legend2[, facet.var[i]] <- factor(
          facet.lev[i],
          levels(data[, facet.var[i]][[1]])
        )
      } else {
        legend2[, facet.var[i]] <- factor(
          facet.lev[i],
          levels(data[, facet.var[i]])
        )
      }
    }
  } else if (!is.null(facet.var) & is.null(facet.lev)) {
    facet.levels0 <- unique(as.data.frame(data)[, facet.var])
    facet.levels <- unlist(unique(as.data.frame(data)[, facet.var]))
    legend2 <- do.call("rbind", replicate(length(facet.levels),
      legend2,
      simplify = FALSE
    ))
    if (length(facet.var) > 1) {
      facet.levels0 <- expand.grid(facet.levels0)
      legend2[, facet.var] <-
        facet.levels0[rep(row.names(facet.levels0), each = 3), ]
    } else {
      legend2[, facet.var] <- rep(facet.levels0, each = 3)
    }
  }
  if (!st.inherit) {
    legend2 <- legend2[, c("x", "y", "label")]
  }
  gg.legend <-
    geom_text(
      data = legend2,
      aes(
        x,
        y,
        label = label
      ),
      size = st.size,
      color = st.color,
      # We must change the font family the hard way!
      inherit.aes = st.inherit,
      family = "Fira Sans Condensed Medium"
    )
  # Right distance for unit label
  if (direction < 0) dist_direction <- 0
  else dist_direction <- 2

  unit_pos <-
    maptools::gcDestination(
      lon = x,
      lat = y,
      bearing = 90, #* direction,
      dist = dist * (dist_direction + unit_pos_dist),
      dist.units = dist_unit,
      model = model
    )[1, 1]

    gg.unit <-
    annotate(
      "text",
      x = unit_pos,
      y = max(legend2$y),
      label = dist_unit,
      family = "Fira Sans Condensed Medium"
    )

  return(list(gg.box1, gg.box2, gg.legend, gg.unit))
}
