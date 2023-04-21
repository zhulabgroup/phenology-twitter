df_tw_st <- df_tweet %>%
  left_join(df_geo_all %>% select(user_location, state), by = "user_location") %>%
  filter(!is.na(state)) %>%
  mutate(date = lubridate::date(paste(year, month, day, sep = "-"))) %>%
  mutate(
    year = lubridate::year(date),
    doy = lubridate::yday(date)
  ) %>%
  # filter(doy > 40, doy <= 180) %>%
  group_by(state, year, doy) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  left_join(df_user_num_sum, by = "year") %>%
  mutate(count_adj = count / user) %>%
  select(-user) %>%
  group_by(state, doy) %>%
  summarise(count_mn = mean(count_adj, na.rm = T)) %>%
  ungroup() %>%
  spread(key = "state", value = "count_mn") %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%
  complete(doy = 1:365) %>% # add NA to gap dates
  gather(key = "state", value = "count_mn", -doy) %>%
  group_by(state) %>%
  mutate(count_tr = count_mn^(1 / 2)) %>%
  # mutate(count_sd = count_tr / quantile(count_mn, 0.95, na.rm = T)) %>%
  mutate(count_in = zoo::na.approx(count_mn, doy, na.rm = F, maxgap = 14)) %>%
  mutate(count_sm = whitfun(count_in, 30)) %>%
  ungroup() %>%
  mutate(tweet = count_sm) %>%
  mutate(state = toupper(state)) %>%
  mutate(state = case_when(
    state == "DC" ~ "district of columbia",
    TRUE ~ state.name[match(state, state.abb)]
  )) %>%
  left_join(data.frame(state = state.x77 %>% rownames(), population = state.x77[, "Population"]), by = "state") %>%
  mutate(state = tolower(state))

df_state_coord <- map_data("state") %>%
  group_by(state = region) %>%
  summarise(
    lon = mean(long),
    lat = mean(lat)
  )

df_tw_doy_state <- bind_rows(
  df_tw_st %>%
    filter(doy > 40, doy <= 180) %>%
    group_by(state) %>%
    arrange(state, doy) %>%
    drop_na() %>%
    filter(cumsum(count_mn) >= 0.3 * sum(count_mn)) %>%
    arrange(doy) %>%
    slice(1) %>%
    ungroup() %>%
    select(state, doy) %>%
    left_join(df_state_coord, by = "state") %>%
    mutate(
      group = "tweet",
      metric = "sos"
    ),
  df_tw_st %>%
    filter(doy > 40, doy <= 180) %>%
    group_by(state) %>%
    arrange(state, doy) %>%
    drop_na() %>%
    filter(cumsum(count_mn) <= 0.7 * sum(count_mn)) %>%
    arrange(desc(doy)) %>%
    slice(1) %>%
    ungroup() %>%
    select(state, doy) %>%
    left_join(df_state_coord, by = "state") %>%
    mutate(
      group = "tweet",
      metric = "eos"
    )
)

v_state_top <- df_tw_st %>%
  group_by(state) %>%
  summarise(count_sum = sum(count_mn, na.rm = T)) %>%
  arrange(desc(count_sum)) %>%
  filter(count_sum >= 500) %>%
  pull(state)

p_tw_pheno_state <- ggplot() +
  geom_line(
    data = df_tw_st %>% filter(state %in% v_state_top),
    aes(x = doy, y = tweet, col = state, group = state)
  ) +
  # geom_vline(data = df_tw_doy_state%>% filter(state %in% v_state_top),
  #            aes(xintercept = doy, col = state))+
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2),
    labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
  ) +
  theme_classic()
