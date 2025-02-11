#' @export
calc_group_ideology <- function(ls_df_group_sample, ls_df_group_valid, ls_df_user_ideology) {
  ls_df_group <- ls_df_group_sample_size <- vector(mode = "list")
  for (group in misc_subset()) {
    df_group_sample <- ls_df_group_sample[[group]]$sample
    total_sample_size <- ls_df_group_sample[[group]]$sample_size

    df_group_valid <- ls_df_group_valid[[group]]

    df_group_valid_ext <- df_group_sample %>%
      select(user = user_screen_name, clean_text) %>%
      right_join(df_group_valid %>% select(clean_text),
        by = "clean_text"
      )

    df_user_ideology <- ls_df_user_ideology[[group]]

    df_group <- df_group_sample %>%
      select(user = user_screen_name, clean_text) %>%
      right_join(df_group_valid %>% select(clean_text),
        by = "clean_text"
      ) %>%
      select(-clean_text) %>%
      distinct(user) %>%
      left_join(
        df_user_ideology,
        by = "user"
      ) %>%
      mutate(group = group) %>%
      filter(ideology != 999) %>%
      filter(!is.na(ideology)) %>%
      filter(is.finite(ideology))

    ls_df_group[[group]] <- df_group

    ls_df_group_sample_size[[group]] <- bind_rows(
      group = group,
      total = total_sample_size,
      sample = df_group_sample %>% distinct(user_screen_name) %>% nrow(),
      valid = df_group_valid_ext %>% distinct(user) %>% nrow(),
      ideo_avail = df_group %>% distinct(user) %>% nrow()
    )
  }

  df_ideo <- bind_rows(ls_df_group) %>%
    mutate(group = factor(group, levels = misc_subset()))

  df_group_sample_size <- bind_rows(ls_df_group_sample_size) %>%
    mutate(rho = sample / total) %>%
    mutate(gamma = ideo_avail / valid) %>%
    mutate(total_valid = valid / rho)

  out <- list(
    sample = df_ideo,
    sample_size = df_group_sample_size
  )

  return(out)
}
