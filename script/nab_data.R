if (!file.exists(str_c(.path$dat_nab, "dat_pollen.rds"))) {
  df_nab <- read_rds(str_c(.path$dat_nab, "dat_20230327.rds"))
  df_taxa <- read_rds(str_c(.path$dat_nab, "taxonomy.rds"))

  df_o <- df_nab %>%
    rename(taxa_raw = taxa) %>%
    left_join(df_taxa, by = "taxa_raw") %>%
    rename(taxa = taxa_clean) %>%
    mutate(kingdom = case_when(
      taxa_raw == "Total Pollen Count" ~ "Viridiplantae",
      TRUE ~ kingdom
    )) %>%
    mutate(family = case_when(
      taxa_raw == "Total Pollen Count" ~ "Total_o",
      TRUE ~ family
    )) %>%
    mutate(genus = case_when(
      taxa_raw == "Total Pollen Count" ~ "Total_o",
      TRUE ~ genus
    )) %>%
    filter(kingdom == "Viridiplantae") %>%
    group_by(date, lat, lon, station, location, id, family, genus, taxa) %>%
    summarise(count = sum(count)) %>%
    ungroup()

  # Not all sites have "total pollen count"
  df_add <- df_o %>%
    filter(family != "Total_o" | is.na(family)) %>%
    group_by(date, lat, lon, station, location, id) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    ungroup() %>%
    mutate(
      family = "Total",
      genus = "Total"
    )

  # left_join(
  #   df_o %>%
  #     filter(family == "Total_o") %>%
  #     select(date, location, station, total_o = count),
  #   df_add %>%
  #     select(date, location, station, total = count),
  #   by = c("date", "location", "station")
  # ) %>%
  #   ggplot() +
  #   geom_point(aes(x = total_o, y = total)) +
  #   theme_classic()

  df_nab_full <- bind_rows(df_o, df_add)
  write_rds(df_nab_full, str_c(.path$dat_nab, "dat_pollen.rds"))
} else {
  df_nab_full <- read_rds(str_c(.path$dat_nab, "dat_pollen.rds"))
}

df_nab_meta <- df_nab_full %>%
  drop_na(count) %>%
  group_by(station, location, lat, lon, id) %>%
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
