#' @include fviz-gda-var.R
NULL
#' Add supplementary individuals.
#'
#' @param res_gda MCA results.
#' @param sup_ind supplementary individual profiles (data.frame).
#' @param colour colour of point and labels.
#' @param label label names.
#' @param size label size.
#' @param group vector containing group definition.
#' @param group_names names of the groups.
#' @param group_style style to plot (vector containing "shape", "colour" or "both).
#' @param ind_visible show individual points.
#' @param axes the GDA dimensions to plot.
#'
#' @return ggplot2 visalization of supplementary individuals.
#' @export
fviz_add_sup_ind <- function(res_gda, sup_ind = NULL, colour = "red", ind_visible = FALSE, label = NULL, size = 10,
                             group = NULL, group_names = NULL, group_style = "both", axes = 1:2) {

  # Datensatz vorbereiten
  colnames(sup_ind) <- colnames(res_gda$call$X)
  res_gda_added <- rbind(res_gda$call$X, sup_ind)

  # Evaluate axes
  axis_1 <- sym(paste0("Dim.", axes[1]))
  axis_2 <- sym(paste0("Dim.", axes[2]))

  # MFA mit passiven Individuen berechnen
  res_sup_ind <-
    MCA(
      res_gda_added[-res_gda$call$ind.sup,],
      excl = res_gda$call$excl,
      ncp = res_gda$call$ncp,
      graph = FALSE,
      ind.sup =
        # @TODO TEST IF LINEBREAK WORKS
        (nrow(res_gda_added[-res_gda$call$ind.sup,]) - nrow(sup_ind) + 1):
        nrow(res_gda_added[-res_gda$call$ind.sup,])
    )

  # Koordinaten extrahieren
  res_sup_ind_coord <- data.frame(res_sup_ind$ind.sup$coord)

  # Repräsentation extrahieren
  # res_sup_ind_coord <- data.frame(res_sup_ind$ind.sup$cos2)

  # Label bezeichnen
  if (is.null(label)) {
    label_names <- rownames(res_sup_ind_coord)
  } else {
    label_names <- label
  }

  # Plotten
  p <-
    fviz_gda_var(
      res_gda,
      group = group,
      group_names = group_names,
      group_style = group_style,
      axes = axes
    )

  if (ind_visible) {
    p <-
      p +
      geom_point(
        data = data.frame(res_gda$ind$coord),
        aes(
          !! axis_1,
          !! axis_2
        ),
        inherit.aes = FALSE,
        alpha = 0.2
      )
  }

  p <-
    p +
    geom_label(
      data = res_sup_ind_coord,
      aes(
        !! axis_1,
        !! axis_2
      ),
      inherit.aes = FALSE,
      size = size,
      colour = "red",
      label = label_names,
      label.size = 1.5
    )

  p

}
