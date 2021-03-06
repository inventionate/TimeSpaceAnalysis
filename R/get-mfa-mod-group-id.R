#' @include get-index-mod.R
NULL
#' Extract the corresponding group id of MFA variable categories.
#'
#' @param res_mfa MFA result.
#'
#' @return vector containing group ids in MFA result order.
#' @export
get_mfa_mod_group_id <- function(res_mfa) {

  # @todo: Hier in Functional Programming switchen.
  group_id <- NULL
  for(g in seq_along(res_mfa$call$group)) {
    group_id <- c(group_id, rep(g, res_mfa$call$group[g]))
  }
  group_id <- tibble(var = colnames(res_mfa$call$X), group_id)
  mod <- tibble(mod = colnames(GDAtools::dichotom(res_mfa$call$X))) %>%
    separate(mod, c("var", "mod"), extra = "merge", sep = "\\.")
  group_shape <- full_join(mod, group_id)
  index_rm <- get_index_mod(res_mfa$call$X, pattern = "Fehlender Wert|kann ich nicht sagen")
  if(!is.null(index_rm)) group_shape <- group_shape[-(index_rm),]
  group_shape <- bind_cols(group_shape, data.frame(res_mfa$quali.var$contrib)) %>%
    # mutate(contrib = Dim.1*res_mfa$eig$eigenvalue[1] + Dim.2*res_mfa$eig$eigenvalue[2]) %>%
    # arrange(desc(contrib)) %>%
    mutate_each(funs(as.character)) %>%
    select(mod, group_id)
  return(group_shape)
}
