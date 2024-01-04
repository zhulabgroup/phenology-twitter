#' @export
read_user_type <- function(path_user_type = "alldata/intermediate/user_type/") {
  ls_df_group_user_type <- vector(mode = "list")
  for (group in misc_subset(forlabel = F)) {
    df_user_type <- read_csv(str_c(path_user_type, group, "_labeled.csv")) %>%
      select(-description) %>%
      gather(key = "type", value = "value", -user) %>%
      drop_na() %>%
      select(-value) %>%
      # mutate(type = case_when (type == "other_organization" | type == "other_individual"~"unofficial",
      #                          TRUE~type)) %>%
      mutate(type = str_replace(type, "_", "\n")) %>%
      mutate(type = factor(type, levels = c("media", "expert", "other\norganization", "other\nindividual")))

    ls_df_group_user_type[[group]] <- df_user_type
  }

  return(ls_df_group_user_type)
}
