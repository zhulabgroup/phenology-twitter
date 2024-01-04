#' @export
summ_group_sample_size <- function(df_group_sample_size, option = "temperature") {
  cond_prob <- (df_group_sample_size %>%
    filter(group == str_c("pollen-", option)) %>%
    pull(total_valid)) / (df_group_sample_size %>%
    filter(group == "pollen") %>%
    pull(total_valid))

  return(cond_prob)
}

#' @export
summ_group_ideology <- function(df_group_ideology) {
  df_ideo_summ <- df_group_ideology %>%
    mutate(lean = case_when(
      ideology <= 0 ~ "left",
      TRUE ~ "right"
    )) %>%
    group_by(group, lean) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    spread(key = "lean", value = "n") %>%
    mutate(total = left + right) %>%
    mutate(
      left = left / total,
      right = right / total
    ) %>%
    select(-total)

  return(df_ideo_summ)
}
