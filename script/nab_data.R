if (!file.exists(str_c(.path$dat_nab, "dat_pollen_total.rds"))) {
  df_count <- tidynab::load_data(request = "2023") %>% 
      tidynab::parse_data() 
  df_taxa <- as.tibble(read.csv(system.file("extdata/2023-04-25", "renew_taxonomy.csv", package = "tidynab")))
  df_geo <- tidynab::geolocate_stations()

  df_pollen_total <- df_count %>%
    left_join(df_taxa, by = c("taxa"="taxa_raw")) %>%
    filter(kingdom == "Viridiplantae") %>%
    group_by(date, stationid) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    ungroup() %>%
    left_join(df_geo, by= c("stationid"="id"))

  write_rds(df_pollen_total, str_c(.path$dat_nab, "dat_pollen_total.rds"))
} else {
  df_pollen_total <- read_rds(str_c(.path$dat_nab, "dat_pollen_total.rds"))
}

df_nab<- df_pollen_total %>% 
  filter(country == "US") %>% 
  filter(!state %in% c("AK", "PR"))

df_nab_meta <- df_nab %>%
  drop_na(count) %>%
  group_by(stationid, name, city, state, lat, lon, country) %>%
  summarise(
    mindate = min(date),
    maxdate = max(date),
    n = n()
  ) %>%
  mutate(range = maxdate - mindate) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(site = NA)

df_nab_meta %>% nrow()

p_nab_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  theme_void() +
  geom_point(data = df_nab_meta, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  coord_map("bonne", lat0 = 50)
