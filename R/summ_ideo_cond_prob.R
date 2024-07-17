#' @export
summ_ideo_cond_prob <- function(df_ideo_cond_prob, option = "climate") {
  df_summ <- df_ideo_cond_prob %>%
    filter(str_detect(group, option)) %>%
    group_by(bin) %>%
    summarise(
      median = median(cond_prob),
      lower = quantile(cond_prob, 0.025),
      upper = quantile(cond_prob, 0.975),
      .groups = "drop"
    ) %>%
    mutate(bin_num = as.numeric(bin))

  return(df_summ)
}
