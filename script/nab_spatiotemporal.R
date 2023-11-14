df_nab_st <- df_nab_filter %>%
  mutate(state = case_when(
    state == "DC" ~ "district of columbia",
    TRUE ~ state.name[match(state, state.abb)]
  )) %>%
  left_join(data.frame(state = state.x77 %>% rownames(), population = state.x77[, "Population"]), by = "state") %>%
  mutate(state = tolower(state)) %>%
  group_by(state, year, doy) %>%
  summarise(count = mean(count, na.rm = T)) %>%
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
  # mutate(count_tr = count_mn^(1 / 2)) %>%
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

df_state_complete <- df_nab_st %>%
  filter(doy > 40, doy <= 180) %>% 
  group_by(state) %>% 
  summarise(sum =sum (pollen, na.rm=F)) %>% 
  drop_na(sum) %>% 
  pull(state)

df_nab_doy_state <- bind_rows(
  df_nab_st %>%
    filter(state %in% df_state_complete) %>% 
    filter(doy > 40, doy <= 180) %>%
    group_by(state) %>%
    arrange(state, doy) %>%
    drop_na() %>%
    filter(cumsum(pollen) >= 0.25 * sum(pollen)) %>%
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
    filter(state %in% df_state_complete) %>% 
    filter(doy > 40, doy <= 180) %>%
    group_by(state) %>%
    arrange(state, doy) %>%
    drop_na() %>%
    filter(cumsum(pollen) <= 0.75 * sum(pollen)) %>%
    arrange(desc(doy)) %>%
    slice(1) %>%
    ungroup() %>%
    select(state, doy) %>%
    left_join(df_state_coord, by = "state") %>%
    mutate(
      group = "pollen",
      metric = "eos"
    ),
  df_nab_st %>%
    filter(state %in% df_state_complete) %>% 
    filter(doy > 40, doy <= 180) %>%
    group_by(state) %>%
    arrange(state, doy) %>%
    drop_na() %>%
    filter(cumsum(pollen) <= 0.5 * sum(pollen)) %>%
    arrange(desc(doy)) %>%
    slice(1) %>%
    ungroup() %>%
    select(state, doy) %>%
    left_join(df_state_coord, by = "state") %>%
    mutate(
      group = "pollen",
      metric = "mos"
    ),
  df_nab_st %>%
    filter(doy > 40, doy <= 180) %>%
    group_by(state) %>%
    arrange(state, doy) %>%
    drop_na() %>%
    arrange(desc(pollen)) %>%
    slice(1) %>%
    ungroup() %>%
    select(state, doy) %>%
    left_join(df_state_coord, by = "state") %>%
    mutate(
      group = "pollen",
      metric = "pos"
    )
)

p_nab_pheno_state <- ggplot() +
  geom_line(
    data = df_nab_st,
    aes(x = doy, y = pollen, col = state, group = state)
  ) +
  geom_vline(xintercept = 40)+
  geom_vline(xintercept = 180)+
  geom_vline(
    data = df_nab_doy_state,
    aes(xintercept = doy, col = state)
  ) +
  # scale_y_continuous(
  #   trans = scales::sqrt_trans(),
  #   breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2),
  #   labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
  # ) +
  facet_wrap(. ~ state, scales = "free_y") +
  guides(col = "none") +
  theme_classic()
