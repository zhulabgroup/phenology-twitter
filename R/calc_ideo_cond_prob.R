#' @export
calc_ideo_cond_prob <- function(df_group_ideology, df_group_sample_size) {
  df_ideo_bin <- df_group_ideology %>%
    mutate(bin = cut(ideology, breaks = seq(-2, 2, by = 0.5))) %>%
    group_by(group, bin) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    drop_na(bin) %>%
    left_join(df_group_sample_size, by = "group") %>%
    mutate(n_est = n / gamma / rho) %>%
    select(group, bin, n = n_est)

  bin_level <- df_ideo_bin$bin %>%
    unique() %>%
    levels()

  df_ideo_cond_prob <- df_ideo_bin %>%
    spread(key = "bin", value = "n") %>%
    gather(key = "bin", value = "n", -group) %>%
    mutate(n = replace_na(n, 0)) %>%
    # group_by(group) %>%
    # mutate(freq = n / sum(n)) %>%
    # ungroup() %>%
    # select(-n) %>%
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
    ))

  return(df_ideo_cond_prob)
}
