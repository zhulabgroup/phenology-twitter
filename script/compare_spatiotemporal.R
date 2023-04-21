df_doy_state <- bind_rows(
  df_nab_doy_state,
  df_tw_doy_state # %>% filter(state %in% v_state_top)
) %>%
  mutate(metric = factor(metric, levels = c("sos", "eos")))

p_doy_state <- ggplot(df_doy_state) +
  geom_point(aes(x = lat, y = doy, col = group, group = group)) +
  geom_smooth(aes(x = lat, y = doy, col = group, group = group), method = "lm") +
  ggpubr::stat_cor(aes(
    x = lat, y = doy, col = group, group = group,
    label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
  )) +
  theme_classic() +
  facet_wrap(. ~ metric)

p_doy_corr <- ggplot(df_doy_state %>%
  spread(key = "group", value = "doy")) +
  geom_point(aes(x = pollen, y = tweet, group = metric)) +
  geom_smooth(aes(x = pollen, y = tweet, group = metric), method = "lm") +
  ggpubr::stat_cor(aes(
    x = pollen, y = tweet, group = metric,
    label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
  )) +
  theme_classic() +
  facet_wrap(. ~ metric, scales = "free")

# animation
df_stmap <- map_data("state") %>%
  full_join(
    full_join(df_tw_st %>% select(state, doy, tweet),
      df_nab_st %>% select(state, doy, pollen),
      by = c("state", "doy")
    ),
    by = c("region" = "state")
  ) %>%
  mutate(
    tweet = ifelse(tweet >= 6^2, 6^2, tweet),
    pollen = ifelse(pollen >= 60^2, 60^2, pollen)
  )

# takes long to run
if (FALSE) {
  v_doy <- 40:180
  for (i in 1:length(v_doy)) {
    doyoi <- v_doy[i]

    tw_breaks <- seq(0, 6, by = 1)^2
    p_tw <- ggplot() +
      geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
      geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
      geom_polygon(data = df_stmap %>% filter(doy == doyoi), aes(x = long, y = lat, group = group, fill = tweet)) +
      theme_void() +
      coord_map("bonne", lat0 = 50) +
      scale_fill_viridis_c(
        option = "magma", direction = -1, limits = c(0, 6^2),
        breaks = tw_breaks, labels = tw_breaks,
        trans = scales::sqrt_trans()
      ) +
      labs(fill = "") +
      ggtitle(str_c("Twitter count on day ", doyoi)) +
      labs(col = "") +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(1.2, "cm")
      )

    nab_breaks <- seq(0, 60, by = 10)^2
    p_nab <- ggplot() +
      geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
      geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
      geom_polygon(data = df_stmap %>% filter(doy == doyoi), aes(x = long, y = lat, group = group, fill = pollen)) +
      theme_void() +
      coord_map("bonne", lat0 = 50) +
      scale_fill_viridis_c(
        option = "magma", direction = -1, limits = c(0, 60^2),
        breaks = nab_breaks, labels = nab_breaks,
        trans = scales::sqrt_trans()
      ) +
      labs(fill = "") +
      ggtitle(str_c("Pollen concentration on day ", doyoi)) +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(1.2, "cm")
      )


    p <- p_tw + p_nab +
      plot_layout(design = "
                AB
                ")

    ggsave(
      plot = p,
      filename = str_c(.path$fig_st, doyoi, ".png"),
      width = 10,
      height = 6
    )

    print(doyoi)
  }
}
