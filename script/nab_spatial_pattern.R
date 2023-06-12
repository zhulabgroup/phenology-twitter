df_nab_station <- df_nab_ts %>%
  filter(doy > 40, doy <= 180) %>%
  group_by(lat, lon, station, location, year) %>%
  summarise(
    count = sum(count),
    days = n()
  ) %>%
  ungroup() %>%
  mutate(intensity = (count / days)^(1 / 2)) %>%
  arrange((intensity)) %>%
  rename(pollen = intensity)

p_nab_map_station <- ggplot() +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  geom_point(data = df_nab_station, aes(x = lon, y = lat, fill = pollen), pch = 21, cex = 5) +
  theme_void() +
  facet_wrap(. ~ year) +
  coord_map("bonne", lat0 = 50) +
  scale_fill_viridis_c(option = "magma", direction = -1)

df_nab_state_name <- df_nab_station %>%
  filter(country == "US") %>%
  filter(state != "PR") %>%
  distinct(id, state) %>%
  rowwise() %>%
  mutate(state = state.name[match(state, state.abb)]) %>%
  mutate(state = tolower(state))

df_nab_state <- df_nab_ts %>%
  left_join(df_nab_state_name, by = "location") %>%
  filter(doy > 40, doy <= 180) %>%
  group_by(state, year) %>%
  summarise(
    count = sum(count),
    days = n()
  ) %>%
  ungroup() %>%
  mutate(intensity = (count / days)^(1 / 2)) %>%
  arrange((intensity)) %>%
  rename(pollen = intensity)

df_nab_map_state <- map_data("state") %>%
  inner_join(df_nab_state, by = c("region" = "state"))

p_nab_map_state <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  geom_polygon(data = df_nab_map_state, aes(x = long, y = lat, group = group, fill = pollen)) +
  theme_void() +
  facet_wrap(. ~ year) +
  coord_map("bonne", lat0 = 50) +
  scale_fill_viridis_c(option = "magma", direction = -1)
