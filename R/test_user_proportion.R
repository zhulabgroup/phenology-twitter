#' @export
test_user_proportion <- function(end = NULL) {
  ls_df_user_proportion <- vector(mode = "list")
  for (group in misc_subset(forlabel = F)) {
    ls_df_user_proportion[[group]] <- tidy_user_proportion(
      df_group_flow = ls_df_group_flow[[group]],
      df_group_user_type = ls_df_group_user_type[[group]],
      end = end
    ) %>%
      mutate(group = group)
  }

  mat_user_proportion <- bind_rows(ls_df_user_proportion) %>%
    spread(key = "group", value = "count") %>%
    column_to_rownames(var = "type") %>%
    as.matrix()

  res <- chisq.test(mat_user_proportion)

  return(res)
}

#' @export
tidy_user_proportion <- function(df_group_flow, df_group_user_type, end = NULL) {
  df <- df_group_flow %>%
    select(user = !!sym(end), count) %>%
    left_join(df_group_user_type, by = "user") %>%
    group_by(type) %>%
    summarise(count = sum(count)) %>%
    drop_na()

  return(df)
}
