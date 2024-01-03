#' @export
calc_spatiotemporal_metrics <- function(df_st = NULL) {
  df_st_metric <- bind_rows(
    df_st %>%
      filter(doy >= 32, doy <= 151) %>%
      group_by(state, state_name) %>%
      arrange(state, state_name, doy) %>%
      drop_na() %>%
      filter(cumsum(count_sm) >= 0.25 * sum(count_sm)) %>%
      arrange(doy) %>%
      slice(1) %>%
      ungroup() %>%
      select(state, state_name, doy) %>%
      mutate(
        metric = "sos"
      ),
    df_st %>%
      filter(doy >= 32, doy <= 151) %>%
      group_by(state, state_name) %>%
      arrange(state, state_name, doy) %>%
      drop_na() %>%
      filter(cumsum(count_sm) <= 0.75 * sum(count_sm)) %>%
      arrange(desc(doy)) %>%
      slice(1) %>%
      ungroup() %>%
      select(state, state_name, doy) %>%
      mutate(
        metric = "eos"
      ),
    df_st %>%
      filter(doy >= 32, doy <= 151) %>%
      group_by(state, state_name) %>%
      arrange(state, state_name, doy) %>%
      drop_na() %>%
      filter(cumsum(count_sm) <= 0.5 * sum(count_sm)) %>%
      arrange(desc(doy)) %>%
      slice(1) %>%
      ungroup() %>%
      select(state, state_name, doy) %>%
      mutate(
        metric = "mos"
      ),
    df_st %>%
      filter(doy >= 32, doy <= 151) %>%
      group_by(state, state_name) %>%
      arrange(state, state_name, doy) %>%
      drop_na() %>%
      arrange(desc(count_sm)) %>%
      slice(1) %>%
      ungroup() %>%
      select(state, state_name, doy) %>%
      mutate(
        metric = "pos"
      )
  ) %>%
    mutate(state_name_lower = tolower(state_name)) %>%
    left_join(
      map_data("state") %>% # state coordinate
        group_by(state = region) %>%
        summarise(
          lon = mean(long),
          lat = mean(lat)
        ),
      by = c("state_name_lower" = "state")
    ) %>%
    select(-state_name_lower)

  return(df_st_metric)
}
