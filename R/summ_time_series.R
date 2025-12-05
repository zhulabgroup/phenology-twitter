#' @export
summ_time_series_twitter <- function(df_ts_tw) {
  df_peak <- df_ts_tw %>%
    group_by(year) %>%
    arrange(desc(count_adj)) %>%
    slice(1) %>%
    ungroup() %>%
    drop_na(count_adj) %>%
    arrange(desc(count_adj))

  return(df_peak)
}
