#' @export
summ_ts_spring <- function(df_ts_tw) {
  df <- df_ts_tw %>%
    mutate(spring = case_when(
      doy >= 32 & doy <= 151 ~ T,
      TRUE ~ F
    )) %>%
    group_by(spring) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    mutate(proportion = count / sum(count)) %>%
    mutate(percentage = round(proportion * 100), str_c("%"))

  return(df)
}
