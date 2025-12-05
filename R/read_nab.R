#' @export
read_nab <- function() {
  df_count <- tidynab::load_data(request = "2023") %>%
    tidynab::parse_data()
  df_taxa <- as.tibble(read.csv(system.file("extdata/2023-04-25", "renew_taxonomy.csv", package = "tidynab")))
  df_geo <- tidynab::geolocate_stations()

  df_nab <- df_count %>%
    mutate(date = lubridate::date(date)) %>%
    left_join(df_taxa, by = c("taxa" = "taxa_raw")) %>%
    filter(kingdom == "Viridiplantae") %>%
    group_by(date, stationid) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    ungroup() %>%
    left_join(df_geo, by = c("stationid" = "id")) %>%
    filter(country == "US")

  return(df_nab)
}

#' @export
plot_nab_map <- function(df_nab, save = F, save_path = "alldata/output/figures/") {
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


  p_nab_map <- ggplot() +
    geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
    geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
    theme_void() +
    geom_point(data = df_nab_meta, aes(x = lon, y = lat), pch = 1, color = "black", cex = 3) +
    coord_map("bonne", lat0 = 50)

  if (save) {
    ggsave(
      plot = p_nab_map,
      filename = str_c(save_path, "supp/nab_map.png"),
      width = 8,
      height = 6,
      device = png,
      type = "cairo"
    )
  }
  return(p_nab_map)
}
