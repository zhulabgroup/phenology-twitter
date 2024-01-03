#' @export
tidy_time_series <- function(df_tweet = NULL, df_user_num = NULL, df_nab = NULL, option = "twitter", process = F) {
  if (option == "twitter") {
    df_ts <- tidy_time_series_twitter(df_tweet = df_tweet, df_user_num = df_user_num, process = process)
  }

  if (option == "nab") {
    df_ts <- tidy_time_series_nab(df_nab = df_nab, process = process)
  }
  return(df_ts)
}

tidy_time_series_nab <- function(df_nab, process = F) {
  df_ts_nab <- df_nab %>%
    mutate(
      doy = lubridate::yday(date),
      year = lubridate::year(date)
    ) %>%
    right_join(group_by(., stationid, year) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      filter(count >= 100) %>% # filter for station and year with more than 100 data points
      select(-count), by = c("stationid", "year")) %>%
    select(stationid, state, city, lat, lon, date, year, doy, count)

  if (process) {
    df_ts_nab <- df_ts_nab %>%
      mutate(count_tr = count^(1 / 2)) %>% # squareroot transformation
      group_by(stationid) %>%
      mutate(count_sd = count_tr / quantile(count_tr, 0.95, na.rm = T)) %>% # standardize to approximately 0 to 1 for each station
      ungroup() %>%
      group_by(year, doy, date) %>%
      summarise(
        count_mn = mean(count_sd), # average over all stations
        n = n()
      ) %>%
      ungroup() %>%
      filter(n >= 5) %>% # must have data from more than five stations
      mutate(count_sm = util_fill_whit(x = count_mn, maxgap = 14, lambda = 30, minseg = 2)) %>%
      select(date, year, doy, count_mn, count_sm)
  }
  return(df_ts_nab)
}

tidy_time_series_twitter <- function(df_tweet, df_user_num, process = F) {
  df_ts_tw <- df_tweet %>%
    mutate(date = lubridate::date(paste(year, month, day, sep = "-"))) %>%
    mutate(doy = lubridate::yday(date)) %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year, doy, date) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    right_join(
      data.frame(date = seq(lubridate::date("2012-01-01"), lubridate::date("2022-12-31"), by = "day")) %>%
        mutate(
          doy = lubridate::yday(date),
          year = lubridate::year(date)
        ),
      by = c("year", "doy", "date")
    ) %>% # add NA to gap dates
    left_join(df_user_num, by = "year") %>%
    mutate(count_adj = count / user) %>%
    select(date, year, doy, count, count_adj)

  if (process) {
    df_ts_tw <- df_ts_tw %>%
      mutate(count_tr = count_adj^(1 / 2)) %>% # squareroot transformation
      mutate(count_sd = count_tr / quantile(count_tr, 0.95, na.rm = T)) %>% # standardize to roughly between 0 and 1
      mutate(count_sm = util_fill_whit(x = count_sd, maxgap = 14, lambda = 30, minseg = 2)) %>%
      select(date, year, doy, count, count_adj, count_tr, count_sd, count_sm)
  }

  return(df_ts_tw)
}
