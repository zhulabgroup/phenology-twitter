#' @export
tidy_remove_sample <- function(df, option) {
  if (option == "nab") {
    df_summ <- df %>%
      mutate(
        doy = lubridate::yday(date),
        year = lubridate::year(date)
      ) %>%
      group_by(stationid, year) %>%
      summarise(count = sum(count)) %>%
      ungroup() %>%
      mutate(type = ifelse(count >= 100, "include", "exclude")) %>%
      group_by(type) %>%
      summarize(sample_size = n())
  }

  if (option == "twitter") {
    df_summ <- df %>%
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
      group_by(state, year) %>%
      summarise(count = sum(count_adj)) %>%
      ungroup() %>%
      mutate(type = ifelse(count >= 10, "include", "exclude")) %>%
      group_by(type) %>%
      summarize(sample_size = n())
  }

  return(df_summ)
}
