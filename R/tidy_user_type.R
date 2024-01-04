read_user_description <- function(df_tweet) {
  df_user_desc <- df_tweet %>%
    group_by(user = user_screen_name) %>%
    summarise(description = user_description[1]) %>%
    drop_na()

  return(df_user_desc)
}

#' @export
tidy_user_type <- function(ls_df_group_flow, df_tweet, path_user_type = "alldata/intermediate/user_type/") {
  df_user_desc <- read_user_description(df_tweet = df_tweet)
  for (group in misc_subset(forlabel = F)) {
    df_flow <- ls_df_group_flow[[group]]

    df_user <- data.frame(user = c(df_flow$to, df_flow$from)) %>%
      drop_na() %>%
      distinct() %>%
      left_join(df_user_desc, by = "user") %>%
      mutate(
        media = "",
        expert = "",
        other_organization = "",
        other_individual = ""
      )

    write_csv(df_user, str_c(path_user_type, group, ".csv"))
  }

  return(path_user_type)
}
