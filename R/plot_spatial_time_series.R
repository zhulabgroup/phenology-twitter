#' @export
plot_spatial_time_series <- function(df_st_tw = NULL, df_st_nab = NULL, df_st_metric_tw = NULL, df_st_metric_nab = NULL, metricoi = "mos", option = "twitter", save = F, save_path = "alldata/output/figures/") {
  if (option == "compare") {
    df_st_compare <- full_join(df_st_tw %>% select(state, state_name, doy, tweet = count_sm),
      df_st_nab %>% select(state, state_name, doy, pollen = count_sm),
      by = c("state", "state_name", "doy")
    ) %>%
      gather(key = "data", value = "count_sm", -state, -state_name, -doy) %>%
      mutate(data = factor(data,
        levels = c("tweet", "pollen"),
        labels = c(
          "tweet count",
          "pollen concentration"
        )
      ))

    df_st_metric_compare <- full_join(df_st_metric_tw %>% select(state, state_name, metric, tweet = doy),
      df_st_metric_nab %>% select(state, state_name, metric, pollen = doy),
      by = c("state", "state_name", "metric")
    ) %>%
      gather(key = "data", value = "doy", -state, -state_name, -metric) %>%
      mutate(data = factor(data,
        levels = c("tweet", "pollen"),
        labels = c(
          "tweet count",
          "pollen concentration"
        )
      ))

    df_state_order <- df_st_metric_tw %>%
      filter(metric == metricoi) %>%
      arrange(doy) %>%
      select(state, doy)

    p_st <- ggplot(df_state_order) +
      geom_rect(aes(fill = doy),
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        alpha = 0.25
      ) +
      geom_line(
        data = df_st_compare,
        aes(x = doy + lubridate::date("2023-01-01") - 1, y = count_sm, col = data, group = data)
      ) +
      geom_vline(
        data = df_st_metric_compare %>%
          filter(metric == metricoi),
        aes(xintercept = doy + lubridate::date("2023-01-01") - 1, col = data, group = data),
        linetype = "dotted"
      ) +
      scale_color_manual(values = c("tweet count" = "dark blue", "pollen concentration" = "dark orange")) +
      scale_fill_viridis_c(
        direction = -1,
        na.value = "white",
        breaks = seq(90, 130, by = 20)
      ) +
      scale_y_continuous(
        trans = scales::sqrt_trans(),
        breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2) # ,
        # labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
      ) +
      scale_x_date(
        date_labels = "%b",
        breaks = seq(lubridate::date("2023-01-01"),
          lubridate::date("2024-01-01"),
          by = "6 months"
        )
      ) +
      theme_classic() +
      labs(
        x = NULL,
        y = "Pollen phenology (standardized value)",
        fill = str_c("Time of spring pollen peak\nderived from \n", "Twitter", " pollen phenology\n(day of year)"),
        col = "Data source"
      ) +
      guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title.hjust = 0)) +
      theme(
        strip.background = element_rect(linetype = "blank", fill = NA),
        strip.text = element_text(
          face = "bold",
          size = 10.5,
          vjust = -0.5
        ),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.spacing.x = unit(3, "points"),
        panel.spacing.y = unit(-2, "points"),
        legend.position = c(0.925, 0.225),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 12.5),
        plot.margin = unit(c(5.5, 8, 4, 5.5), "pt")
      ) +
      geofacet::facet_geo(~state, scales = "free_y", grid = us_state_grid1)
  }

  if (option == "twitter" | option == "nab") {
    if (option == "twitter") {
      df_st <- df_st_tw
      df_st_metric <- df_st_metric_tw
    }
    if (option == "nab") {
      df_st <- df_st_nab
      df_st_metric <- df_st_metric_nab
    }

    df_state_order <- df_st_metric %>%
      filter(metric == metricoi) %>%
      arrange(doy) %>%
      select(state, doy)

    p_st <- ggplot(df_state_order) +
      geom_rect(aes(fill = doy),
        xmin = -Inf, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        alpha = 0.25
      ) +
      geom_line(
        data = df_st,
        aes(x = doy + lubridate::date("2023-01-01") - 1, y = count_sm)
      ) +
      geom_vline(
        data = df_st_metric %>%
          filter(metric == metricoi),
        aes(xintercept = doy + lubridate::date("2023-01-01") - 1),
        linetype = "dotted"
      ) +
      scale_fill_viridis_c(
        direction = -1,
        na.value = "white",
        breaks = seq(90, 130, by = 20)
      ) +
      scale_y_continuous(
        trans = scales::sqrt_trans(),
        breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2)
      ) +
      scale_x_date(
        date_labels = "%b",
        breaks = seq(lubridate::date("2023-01-01"),
          lubridate::date("2024-01-01"),
          by = "6 months"
        )
      ) +
      theme_classic() +
      labs(
        x = NULL,
        y = case_when(
          option == "twitter" ~ "Twitter pollen phenology (standardized value)",
          option == "nab" ~ "Natural pollen phenology (standardized value)"
        ),
        fill = str_c(
          "Time of spring pollen peak\nderived from \n",
          case_when(
            option == "twitter" ~ "Twitter",
            option == "nab" ~ "natural"
          ),
          " pollen phenology\n(day of year)"
        )
      ) +
      guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title.hjust = 0)) +
      theme(
        strip.background = element_rect(linetype = "blank", fill = NA),
        strip.text = element_text(
          face = "bold",
          size = 10.5,
          vjust = -0.5
        ),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.spacing.x = unit(3, "points"),
        panel.spacing.y = unit(-2, "points"),
        legend.position = c(0.925, 0.225),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 12.5),
        plot.margin = unit(c(5.5, 8, 4, 5.5), "pt")
      ) +
      geofacet::facet_geo(~state, scales = "free_y", grid = us_state_grid1)
  }


  if (save) {
    ggsave(
      plot = p_st,
      filename = str_c(save_path, "supp/spatiotemporal_", option, ".png"),
      width = 12,
      height = 12 * 0.618,
      device = png,
      type = "cairo"
    )
  }

  return(p_st)
}
