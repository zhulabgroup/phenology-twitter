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
