#' @export
tidy_information_flow <- function(ls_df_group_sample, ls_df_group_valid, path_flow = "alldata/intermediate/network/", writecsv = F) {
  ls_df_group_flow <- vector(mode = "list")
  for (group in misc_subset(forlabel = F)) {
    df_sample <- ls_df_group_sample[[group]]$sample
    df_valid <- ls_df_group_valid[[group]]

    df_retweet <- df_sample %>%
      select(user = user_screen_name, clean_text, text, type) %>%
      inner_join(df_valid %>% select(clean_text),
        by = "clean_text"
      ) %>%
      mutate(retweet = str_extract_all(text, "RT @\\w+", simplify = F))

    df_flow <- bind_rows(
      df_retweet %>%
        rowwise() %>%
        filter(length(retweet) == 0) %>%
        mutate(retweet = NA) %>%
        ungroup(),
      df_retweet %>%
        rowwise() %>%
        filter(length(retweet) > 0) %>%
        ungroup() %>%
        unnest_longer(retweet)
    ) %>%
      mutate(retweet = str_replace(retweet, "RT @", "")) %>%
      # select(to = user, from = mention) %>%
      group_by(to = user, from = retweet) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      arrange(desc(count)) %>%
      drop_na()

    ls_df_group_flow[[group]] <- df_flow

    if (writecsv) {
      write_csv(df_flow, str_c(path_flow, group, ".csv"))
    }
  }

  return(ls_df_group_flow)
}
