# df_nab_full <- read_rds(str_c(.path$dat_nab, "dat_pollen.rds"))

df_nab_ts <- df_nab %>%
  mutate(
    doy = lubridate::yday(date),
    year = lubridate::year(date)
  )

# Select for station and year with sample size
# df_nab_ts %>%
#   group_by(id, year) %>%
#   summarise(count = n()) %>%
#   pull(count) %>%
#   hist()

df_site_year <- df_nab_ts %>%
  group_by(stationid, year) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count >= 100) %>%
  select(-count)

df_nab_filter <- df_nab_ts %>%
  right_join(df_site_year, by = c("stationid", "year"))

p_nab_ts <- ggplot(df_nab_filter) +
  geom_line(aes(x = doy, y = count, group = year, col = year)) +
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2),
    labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
  ) +
  facet_wrap(. ~ str_c(state, ", ", city), scales = "free_y") +
  theme_classic() +
  scale_color_viridis_c()

df_nab_ts_fs <- df_nab_filter %>%
  group_by(stationid, city, state) %>%
  mutate(count_in = zoo::na.approx(count, date, na.rm = F, maxgap = 14)) %>%
  mutate(count_sm = whitfun(count_in, 50)) %>%
  mutate(doy = lubridate::yday(date)) %>%
  ungroup()

p_nab_ts_fs <- ggplot(df_nab_ts_fs) +
  geom_line(aes(x = doy, y = count_sm, group = year, col = year)) +
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2),
    labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
  ) +
  facet_wrap(. ~ str_c(state, ", ", city), scales = "free_y") +
  theme_classic() +
  scale_color_viridis_c()


# Average it out
df_nab_ts_sd <- df_nab_filter %>%
  mutate(count_tr = count^(1 / 2)) %>%
  group_by(stationid) %>%
  mutate(count_sd = count_tr / quantile(count_tr, 0.95, na.rm = T)) %>%
  ungroup() %>%
  group_by(year, doy, date) %>%
  summarise(
    count_mn = mean(count_sd),
    n = n()
  ) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  mutate(count_in = zoo::na.approx(count_mn, date, na.rm = F, maxgap = 14)) %>%
  mutate(count_sm = whitfun(count_in, 30)) %>%
  mutate(pollen = count_sm)

p_nab_ts_sd <- ggplot(df_nab_ts_sd) +
  geom_line(aes(x = doy, y = pollen, group = year, col = year)) +
  theme_classic() +
  scale_color_viridis_c()
