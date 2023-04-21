df_nab_st <- df_nab_ts %>%
  left_join(df_nab_state_name, by = "location") %>%
  group_by(state, year, doy) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  group_by(state, doy) %>%
  summarise(
    count_mn = mean(count, na.rm = T),
    n = n()
  ) %>%
  ungroup() %>%
  filter(n >= 3) %>%
  group_by(state) %>%
  complete(doy = 1:365) %>% # add NA to gap dates
  mutate(count_tr = count_mn^(1 / 2)) %>%
  # mutate(count_sd = count_tr / quantile(count_mn, 0.95, na.rm = T)) %>%
  mutate(count_in = zoo::na.approx(count_mn, doy, na.rm = F, maxgap = 14)) %>%
  mutate(count_sm = whitfun(count_in, 30)) %>%
  ungroup() %>%
  mutate(pollen = count_sm)

df_state_coord <- map_data("state") %>%
  group_by(state = region) %>%
  summarise(
    lon = mean(long),
    lat = mean(lat)
  )

df_nab_doy_state <- bind_rows(
  df_nab_st %>%
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
      group = "pollen",
      metric = "sos"
    ),
  df_nab_st %>%
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
      group = "pollen",
      metric = "eos"
    )
)

p_nab_pheno_state <- ggplot() +
  geom_line(
    data = df_nab_st,
    aes(x = doy, y = pollen, col = state, group = state)
  ) +
  # geom_vline(data = df_tw_doy_state%>% filter(state %in% v_state_top),
  #            aes(xintercept = doy, col = state))+
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2),
    labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
  ) +
  theme_classic()
