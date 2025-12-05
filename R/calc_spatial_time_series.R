#' @export
calc_spatial_time_series <- function(df_tweet = NULL, df_user_num = NULL, df_nab = NULL, option = "twitter") {
  if (option == "twitter") {
    df_st <- calc_spatial_time_series_twitter(df_tweet = df_tweet, df_user_num = df_user_num)
  }

  if (option == "nab") {
    df_st <- calc_spatial_time_series_nab(df_nab = df_nab)
  }
  return(df_st)
}

calc_spatial_time_series_nab <- function(df_nab) {
  df_st_nab <- df_nab %>%
    mutate(
      year = lubridate::year(date),
      doy = lubridate::yday(date)
    ) %>%
    right_join(group_by(., stationid, year) %>%
      summarise(count = sum(count)) %>%
      ungroup() %>%
      filter(count >= 100) %>%
      select(-count), by = c("stationid", "year")) %>%
    group_by(state, year, doy) %>%
    summarise(count = mean(count, na.rm = T)) %>% # average over stations in each state
    ungroup() %>%
    group_by(state, doy) %>%
    summarise(
      count_mn = mean(count, na.rm = T), # long-term mean
      n = n()
    ) %>%
    ungroup() %>%
    group_by(state) %>%
    complete(doy = 1:365) %>% # add NA to gap dates
    mutate(count_sd = count_mn / quantile(count_mn, 0.95, na.rm = T)) %>%
    mutate(count_sm = util_fill_whit(x = count_sd, maxgap = 14, lambda = 30, minseg = 2)) %>%
    ungroup() %>%
    mutate(state_name = case_when(
      state == "DC" ~ "District of Columbia",
      TRUE ~ state.name[match(state, state.abb)]
    )) %>%
    drop_na(state)

  return(df_st_nab)
}

calc_spatial_time_series_twitter <- function(df_tweet, df_user_num) {
  df_st_tw <- df_tweet %>%
    filter(!is.na(state)) %>%
    mutate(date = lubridate::date(paste(year, month, day, sep = "-"))) %>%
    mutate(
      year = lubridate::year(date),
      doy = lubridate::yday(date)
    ) %>%
    group_by(state, year, doy) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    left_join(df_user_num, by = "year") %>%
    mutate(count_adj = count / user) %>%
    select(-user) %>%
    right_join(
      group_by(., state, year) %>%
        summarise(count = sum(count_adj)) %>%
        ungroup() %>%
        filter(count >= 10) %>%
        select(-count),
      by = c("state", "year")
    ) %>%
    select(year, doy, state, count_adj) %>%
    spread(key = "state", value = "count_adj") %>%
    mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% # add 0 when a state has no tweet on a day when other states have tweets on the same day
    gather(key = "state", value = "count_adj", -year, -doy) %>%
    group_by(state, doy) %>%
    summarise(count_mn = mean(count_adj, na.rm = T)) %>% # take long-term average for each state
    ungroup() %>%
    group_by(state) %>%
    complete(doy = 1:365) %>% # add NA to gap dates
    mutate(count_sd = count_mn / quantile(count_mn, 0.95, na.rm = T)) %>%
    mutate(count_sm = util_fill_whit(x = count_sd, maxgap = 14, lambda = 30, minseg = 2)) %>%
    ungroup() %>%
    mutate(state = toupper(state)) %>%
    mutate(state_name = case_when(
      state == "DC" ~ "District of Columbia",
      TRUE ~ state.name[match(state, state.abb)]
    )) %>%
    drop_na(state)

  return(df_st_tw)
}
