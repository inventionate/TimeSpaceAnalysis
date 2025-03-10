#' Calculate results for supplementary variables.
#'
#' @param res_gda GDA result.
#' @param var_quali_df the supplementary data frame.
#' @param var_quali supplementary variable name (string).
#' @param impute impute missing data (boolean).
#' @param impute_ncp number of dimensions to predict missing values.
#'
#' @return Returns a list:
#' \item{weight}{numeric vector of categories weights}
#' \item{cord}{data frame of categories coordinates}
#' \item{cos2}{data frame of categories square cosine}
#' \item{var}{data frame of categories within variances, variance between and within categories and variable square correlation ratio (eta2)}
#' \item{v.test}{data frame of categories test-values}
#' \item{supvar}{vector of the supplementary variable categories}
#' @export
supvar_stats <- function(res_gda, var_quali_df, var_quali, impute = TRUE, impute_ncp = 2) {

  # Datensatz auslesen
  var <-
    var_quali_df %>%
    select(var = {{ var_quali }}) %>%
    mutate_all(~ as.character(.))

  # Check, ob es fehlende Werte gibt und ggf. imputieren
  if (length(which(is.na(var))) != 0 & impute) {

    message("Info: Missing data will be imputed!")

    # Nur aktive Individuen verwenden
    if (!is.null(res_gda$call$ind.sup)) {
      X <- res_gda$call$X[-res_gda$call$ind.sup,]
    } else {
      X <- res_gda$call$X
    }

    var <- var %>% mutate_all(as.factor)

    if (inherits(res_gda, c("MCA"))) {

      var_impute <- missMDA::imputeMCA(data.frame(X, var$var), ncp = impute_ncp)

    }

    if (inherits(res_gda, c("MFA"))) {

      warning("MFA input. Variances, cos2 and v.test aren't calculated!", call. = FALSE)

      var_impute <- missMDA::imputeMFA(data.frame(X, var$var),
                       c(res_gda$call$group, 1),
                       res_gda$call$ncp,
                       c(res_gda$call$type, "n"))

    }
    # Spalte in Vektor umwandeln
    var <- var_impute$completeObs$var
  } else {

    # Spalte in Vektor umwandeln
    var %<>% magrittr::extract2("var")

    # Fehlende Werte durch Kategorie ersetzen (falls nicht imputiert wurde).
    var[is.na(var)] <- "Fehlender Wert"
  }

  # Adaptiert von GDAtools.
  row_weight <- res_gda$call$row.w
  if(inherits(res_gda, c("MFA"))) row_weight <- res_gda$call$row.w.init
  # Die Gewichte der Zeilen korrigieren, da die MFA diese ausgleicht, was zur Varianzberechnung
  # nicht korrekt ist.
  #if(inherits(res_gda, c("MFA"))) row_weight <- res_gda$call$row.w.init
  n <- sum(row_weight)
  v <- factor(var)
  FK <- colSums(row_weight*(GDAtools::dichotom(as.data.frame(v),out='numeric')))/n
  wt <- row_weight
  # Hier direkt alle Individuen aus dem GDA Ergebnis
  ind <- data.frame(res_gda$ind$coord[,1:res_gda$call$ncp])
  coord <- aggregate(wt*ind,list(v),sum)[,-1]/n/FK
  vrc <- aggregate(wt*ind*ind,list(v),sum)[,-1]/n/FK-coord*coord
  #if(inherits(res_gda, c("MCA")))
  for (i in 1:res_gda$call$ncp) {
    coord[,i] <- coord[,i]/res_gda$svd$vs[i]
  }
  cos2 <- coord*coord/((1/FK)-1)
  weight=n*FK

  names(weight) <- levels(v)
  rownames(coord) <- levels(v)
  rownames(cos2) <- levels(v)
  # Die within variance entspricht dem gewichteten Mittelwert der Unterpunktwolken
  # (vgl. Le Roux/Rouanet 2004: 103)
  wi <- apply(vrc,2,weighted.mean,w=weight)
  be <- res_gda$eig[[1]][1:res_gda$call$ncp]-wi
  # Für Prozentangabe mal 100 machen!
  eta2 <- be/res_gda$eig[[1]][1:res_gda$call$ncp]
  vrc <- rbind(vrc,wi,be,res_gda$eig[[1]][1:res_gda$call$ncp],eta2)
  vrc <- round(vrc,6)
  rownames(vrc) <- c(levels(v),'within','between','total','eta2')
  coord <- round(coord,6)
  v.test <- sqrt(cos2)*sqrt(length(v)-1)
  v.test <- (((abs(coord)+coord)/coord)-1)*v.test

  # Standardisierte Distanzen hinzufügen
  gda_dist <- function(dim, res_gda, coord) {
        dist_make(
            coord[dim] %>% as.matrix(),
            function (v1, v2) abs((v1 - v2)/sqrt(as.data.frame(res_gda$eig)$eigenvalue[dim]))
        )
  }

  dim_n <- res_gda$call$ncp

  dims <- setNames(1:dim_n, paste0("Dim.", 1:dim_n))

  dist <-
      dims %>%
      map(~ gda_dist(., res_gda, coord))

  # Hypothetische Beiträge berechnen
  # Gesamtsumme berechnen
  if (is_null(res_gda$call$row.sup)) {
      var_weight <- fct_count(res_gda$call$X[, 1])[1, 2]
  } else {
      var_weight <- fct_count(res_gda$call$X[-res_gda$call$row.sup, 1])[1, 2]
  }

  n_abs <- as.numeric(var_weight * 100 / res_gda$call$marge.col[1])

  # Hypothetische Beiträge brechnen
  ctr <-
      coord %>%
      rownames_to_column("mod") %>%
      as_tibble() %>%
      add_column(weight = weight) %>%
      mutate(
          across(starts_with("Dim"), ~ .x^2 * weight * 100 / n_abs / as.data.frame(res_gda$eig)$eigenvalue[as.numeric(str_remove(cur_column(), "Dim."))] * 100)
      ) %>%
      select(-weight) %>%
      column_to_rownames("mod")

  # Absolutes Gewicht bei der MFA wiederherstellen
  if (inherits(res_gda, c("MFA"))) {
    list(
      supvar = var,
      weight = round(weight, 1),
      coord = coord,
      dist = dist,
      ctr = ctr
    )
  } else {
    list(
      supvar = var,
      weight = round(weight, 1),
      coord = coord,
      cos2 = round(cos2, 6),
      var = round(vrc, 6),
      v.test = round(v.test, 6),
      dist = dist,
      ctr = ctr
    )
  }
}
