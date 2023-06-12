df_nab <- read_rds(str_c(.path$dat_nab, "2023-04-25/nab_renew.rds"))
df_nab_station <- read_csv(str_c(.path$dat_nab, "2023-04-25/renew_station_info.csv"))
df_nab_taxa <- read_csv(str_c(.path$dat_nab, "2023-04-25/renew_taxonomy.csv"))

df_nab_full <- df_nab %>%
  rename(taxa_raw = taxa) %>%
  left_join(df_nab_taxa, by = "taxa_raw") %>%
  rename(taxa = taxa_clean) %>%
  filter(kingdom == "Viridiplantae") %>%
  group_by(stationid, date) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  filter(
    date >= lubridate::date("2012-01-01"),
    date <= lubridate::date("2022-12-31")
  ) %>%
  rename(id = stationid) %>%
  left_join(df_nab_station %>% select(id, state, country), by = "id") %>%
  filter(country == "US") %>%
  filter(state != "PR") %>%
  select(-country, -state)

station_num <- df_nab_full %>%
  distinct(id) %>%
  nrow()

p_nab_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  theme_void() +
  geom_point(
    data = df_nab_full %>%
      group_by(id) %>%
      summarise(n = n()) %>%
      left_join(df_nab_station %>% select(id, lat, lon), by = "id"),
    aes(x = lon, y = lat, size = n), pch = 1
  ) +
  coord_map("bonne", lat0 = 50) +
  labs(size = "Sample size") +
  theme(legend.position = "bottom")
