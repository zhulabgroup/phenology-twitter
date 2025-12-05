#' @export
plot_phenology_metrics_compare <- function(df_st_metric_tw, df_st_metric_nab, metricoi = "mos", save = F, save_path = "alldata/output/figures/") {
  df_st_metric_compare <- full_join(
    df_st_metric_tw %>%
      filter(metric == metricoi) %>%
      select(state, state_name, lon, lat, tweet = doy),
    df_st_metric_nab %>%
      filter(metric == metricoi) %>%
      select(state, state_name, lon, lat, pollen = doy),
    by = c("state", "state_name", "lon", "lat")
  )

  df_coefficient <- test_st_gradient(df_st_metric_compare) %>%
    mutate(across(1:3, ~ signif(., 3))) %>%
    mutate(
      data = factor(data,
        levels = c("tweet", "pollen"),
        labels = c("tweet count", "pollen concentration")
      )
    )

  annotation_data <- df_coefficient %>%
    mutate(
      x = 30, # Fixed x-position
      y = c(140, 130), # Different y-values for "tweet" and "pollen"
      label = sprintf(
        "italic(beta) == %s~`(`~%s~`,`~%s~`)`",
        slope, slope_lower, slope_upper
      )
    )

  p_st_gradient <- df_st_metric_compare %>%
    gather(key = "data", value = "doy", -state, -state_name, -lon, -lat) %>%
    mutate(data = factor(data,
      levels = c("tweet", "pollen"),
      labels = c(
        "tweet count",
        "pollen concentration"
      )
    )) %>%
    ggplot() +
    geom_point(aes(x = lat, y = doy, col = data, group = data)) +
    geom_smooth(aes(x = lat, y = doy, col = data, group = data, fill = data),
      method = "lm", se = T, alpha = 0.1,
      show.legend = F
    ) +
    scale_color_manual(values = c("tweet count" = "dark blue", "pollen concentration" = "dark orange")) +
    scale_fill_manual(values = c("tweet count" = "dark blue", "pollen concentration" = "dark orange")) +
    geom_text(
      data = annotation_data,
      aes(x = x, y = y, label = label, col = data),
      parse = TRUE,
      hjust = 0,
      size = 4,
      show.legend = F
    ) +
    labs(
      x = "Latitude of state (° N)",
      y = "Time of spring pollen peak\n(day of year)",
      col = "Data source"
    ) +
    theme(legend.position = "bottom")

  v_state_label <- c("Texas", "Georgia", "North Carolina", "California", "New York")
  p_st_corr <- df_st_metric_compare %>%
    drop_na() %>%
    mutate(state_label = case_when(
      state_name %in% v_state_label ~ state_name
    )) %>%
    ggplot() +
    geom_point(aes(x = pollen, y = tweet)) +
    ggrepel::geom_label_repel(
      aes(
        x = pollen,
        y = tweet,
        label = state_label
      ),
      size = 3,
      color = "black",
      fill = NA,
      min.segment.length = 0,
      max.overlaps = Inf,
      label.padding = unit(.25, "lines"),
      label.size = NA
    ) +
    # ggrepel::geom_label_repel(aes(x = pollen, y = tweet, label = state_label), fill = NA) +
    geom_smooth(aes(x = pollen, y = tweet), method = "lm", se = T, alpha = 0.1) +
    ggpubr::stat_cor(
      aes(
        x = pollen, y = tweet,
        label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")
      ),
      p.accuracy = 0.001,
      digits = 3
    ) +
    labs(
      x = "Time of spring pollen peak\nderived from natural pollen phenology\n(day of year)",
      y = "Time of spring pollen peak\nderived from Twitter pollen phenology\n(day of year)",
      col = "Phenological metric"
    )

  if (save) {
    ggsave(
      plot = p_st_gradient,
      filename = str_c(save_path, "supp/phenology_metrics_gradient.png"),
      width = 6,
      height = 6,
      device = png,
      type = "cairo"
    )

    ggsave(
      plot = p_st_corr,
      filename = str_c(save_path, "supp/phenology_metrics_corr.png"),
      width = 6,
      height = 6,
      device = png,
      type = "cairo"
    )
  }

  out <- list(
    gradient = p_st_gradient,
    corr = p_st_corr
  )

  return(out)
}
