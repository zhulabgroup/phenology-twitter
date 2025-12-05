#' @export
calc_ideo_cond_prob <- function(df_group_ideology, df_group_sample_size, R = 1000) {
  set.seed(1)
  n <- nrow(df_group_ideology)

  ls_df_ideo_cond_prob <- vector(mode = "list", length = R)
  for (r in 1:R) {
    df_ideo_bin <- df_group_ideology %>%
      sample_n(n, replace = TRUE) %>% # Resample with replacement
      mutate(bin = cut(ideology, breaks = seq(-2, 2, by = 0.5))) %>%
      group_by(group, bin) %>%
      summarise(n = n(), .groups = "drop") %>%
      drop_na(bin) %>%
      left_join(df_group_sample_size, by = "group") %>%
      mutate(n_est = n / gamma / rho) %>%
      select(group, bin, n = n_est)

    bin_level <- df_ideo_bin$bin %>%
      unique() %>%
      levels()

    ls_df_ideo_cond_prob[[r]] <- df_ideo_bin %>%
      spread(key = "bin", value = "n") %>%
      gather(key = "bin", value = "n", -group) %>%
      mutate(n = replace_na(n, 0)) %>%
      spread(key = "group", value = "n") %>%
      mutate(
        `climate | pollen` = `pollen-climate` / pollen,
        `temperature | pollen` = `pollen-temperature` / pollen
      ) %>%
      select(-pollen, -`pollen-climate`, -`pollen-temperature`) %>%
      gather(key = "group", value = "cond_prob", -bin) %>%
      mutate(bin = factor(bin, levels = bin_level)) %>%
      mutate(group = factor(group,
        levels = c(
          "temperature | pollen",
          "climate | pollen"
        )
      )) %>%
      mutate(sample = r)
  }

  df_ideo_cond_prob <- bind_rows(ls_df_ideo_cond_prob)

  return(df_ideo_cond_prob)
}
