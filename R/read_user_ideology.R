#' @export
read_user_ideology <- function(path_ideo = "alldata/intermediate/ideology/") {
  ls_df_user_ideology <- vector(mode = "list")
  for (group in misc_subset()) {
    ls_df_user_ideology[[group]] <- read_rds(str_c(path_ideo, group, ".rds")) %>%
      select(user, ideology = ideology2)
  }
  return(ls_df_user_ideology)
}
