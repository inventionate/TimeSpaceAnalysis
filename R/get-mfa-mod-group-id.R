#' Extract the corresponding group id of MFA variable categories.
#'
#' @param res_mfa MFA result.
#' @param df_mfa MFA input data frame-
#' @param groups_mfa MFA input group definition.
#' @param axes select axes to extract.
#' @param contrib select contrib level to extract.
#'
#' @return vector containing group ids in MFA result order.
#' @export
#'
#' @examples
get_mfa_mod_group_id <- function(res_mfa, df_mfa, groups_mfa, axes = 1:2, contrib = 25) {
  group_id <- NULL
  for(g in 1:length(groups_mfa)) {
    group_id <- c(group_id, rep(g, groups_mfa[g]))
  }
  group_id <- data_frame(var = colnames(res_mfa$call$X), group_id)
  mod <- data_frame(mod = colnames(dichotom(df_mfa))) %>%
    separate(mod, c("var", "mod"), extra = "merge", sep = "\\.")
  group_shape <- full_join(mod, group_id)
  group_shape <- group_shape[-(get_index_mod(df_mfa, pattern = "Fehlender Wert|kann ich nicht sagen")),]
  group_shape <- bind_cols(group_shape, data.frame(res_mfa$quali.var$contrib)) %>%
    mutate(contrib = Dim.1*res_mfa$eig$eigenvalue[1] + Dim.2*res_mfa$eig$eigenvalue[2]) %>%
    arrange(desc(contrib))
  return(group_shape$group_id[1:contrib])
}