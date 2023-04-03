nab_station <- nab_with_taxa_df %>%
  filter(family == "Total") %>%
  mutate(year = year(date)) %>%
  full_join(site_year, by = c("id", "year")) %>%
  select(lat, lon, location, year, date, state, count) %>%
  group_by(lat, lon, location, year) %>%
  summarise(
    count = sum(count),
    days = n()
  ) %>%
  ungroup() %>%
  mutate(intensity = (count / days)^(1 / 2)) %>%
  select(lat, lon, location, year, intensity) %>%
  # spread(key="year", value="intensity") %>%
  # gather(key="year", value="intensity",-lat, -lon, -location) %>%
  arrange((intensity)) %>%
  rename(pollen = intensity)
nab_station

p_pollen_map <- ggplot() +
  # geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  # geom_polygon(data = df_nab_map, aes(x = long, y = lat, group = group, fill=pollen)) +
  geom_point(data = nab_station %>% filter(year >= 2009, year <= 2017), aes(x = lon, y = lat, fill = pollen), pch = 21, cex = 5) +
  theme_void() +
  facet_wrap(. ~ year) +
  # geom_point(data = meta_df, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  coord_map("bonne", lat0 = 50) +
  scale_fill_viridis_c(option = "magma", direction = -1)
p_pollen_map

nab_state_name <- nab_with_taxa_df %>%
  distinct(location) %>%
  rowwise() %>%
  mutate(state = str_split(location, pattern = ",", simplify = T)[2]) %>%
  mutate(state = str_replace_all(state, " ", "")) %>%
  mutate(state = state.name[match(state, state.abb)]) %>%
  mutate(state = tolower(state))

df_nab_state <- nab_with_taxa_df %>%
  filter(family == "Total") %>%
  drop_na(count) %>%
  mutate(year = year(date)) %>%
  full_join(site_year, by = c("id", "year")) %>%
  left_join(nab_state_name, by = "location") %>%
  select(location, year, date, state, count) %>%
  group_by(state, year) %>%
  summarise(
    count = sum(count),
    days = n()
  ) %>%
  ungroup() %>%
  mutate(intensity = (count / days)^(1 / 2)) %>%
  select(state, year, intensity) %>%
  # spread(key="year", value="intensity") %>%
  # gather(key="year", value="intensity",-state) %>%
  arrange((intensity)) %>%
  rename(pollen = intensity)
df_nab_state

df_nab_map <- map_data("state") %>%
  inner_join(df_nab_state, by = c("region" = "state"))

p_pollen_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  geom_polygon(data = df_nab_map %>% filter(year >= 2009, year <= 2017), aes(x = long, y = lat, group = group, fill = pollen)) +
  theme_void() +
  facet_wrap(. ~ year) +
  # geom_point(data = meta_df, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  coord_map("bonne", lat0 = 50) +
  scale_fill_viridis_c(option = "magma", direction = -1)
p_pollen_map

df_nab_state
df_tw_state

df_state_compare <- df_tw_state %>%
  full_join(df_nab_state, by = c("state", "year")) %>%
  drop_na()
df_state_compare

ggplot(df_state_compare %>% filter(year <= 2017)) +
  geom_point(aes(x = pollen, y = tweet, col = year, group = year), alpha = 0.5) +
  geom_smooth(aes(x = pollen, y = tweet, col = year, group = year), method = "lm", se = F) +
  theme_classic() +
  scale_color_viridis_c()

library(nlme)
lm(data = df_state_compare %>% filter(year <= 2017), tweet ~ pollen) %>% summary()
lme(data = df_state_compare %>% filter(year <= 2017), tweet ~ pollen, random = ~ 1 | year) %>% summary()
