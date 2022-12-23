nab_path<-"/nfs/turbo/seas-zhukai/phenology/RS4flower/NAB/"
nab_df <- read_rds(paste0(nab_path,"nab_dat.rds"))
nab_taxa_df <- read_rds(paste0(nab_path,"nab_taxa.rds"))

nab_with_taxa_df <- nab_df %>%
  rename(taxa_raw = taxa) %>%
  left_join(nab_taxa_df, by = "taxa_raw") %>%
  rename(taxa = taxa_clean) %>%
  mutate(family = case_when(
    taxa_raw == "Total Pollen Count" ~ "Total",
    TRUE ~ family
  )) %>%
  mutate(genus = case_when(
    taxa_raw == "Total Pollen Count" ~ "Total",
    TRUE ~ genus
  )) %>%
  filter(kingdom == "Viridiplantae" | is.na(kingdom)) %>%
  group_by(Date, lat, lon, station, location, id, family, genus, taxa) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(date = as.Date(Date)) %>%
  dplyr::select(-Date)

meta_df <- nab_with_taxa_df %>%
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

p_pollen_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  theme_void() +
  geom_point(data = meta_df, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  coord_map("bonne", lat0 = 50)
p_pollen_map
