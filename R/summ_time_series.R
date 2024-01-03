#' @export
summ_time_series_twitter <- function(df_ts_tw) {
  # df_ts_sum <- bind_rows(
  #   df_ts_tw %>%
  #     summarise(
  #       sum = sum(count_adj, na.rm = T),
  #       mean = mean(count_adj, na.rm = T)
  #     ) %>%
  #     mutate(window = "all"),
  #   df_ts_tw %>%
  #     filter(doy > 40 & doy <= 180) %>%
  #     summarise(
  #       sum = sum(count_adj, na.rm = T),
  #       mean = mean(count_adj, na.rm = T)
  #     ) %>%
  #     mutate(window = "spring")
  # )

  df_peak <- df_ts_tw %>%
    group_by(year) %>%
    arrange(desc(count_adj)) %>%
    slice(1) %>%
    ungroup() %>%
    drop_na(count_adj) %>%
    arrange(desc(count_adj))

  return(df_peak)
}
