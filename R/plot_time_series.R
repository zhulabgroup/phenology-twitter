#' @export
plot_time_series <- function(df_ts_tw = NULL, df_ts_nab = NULL, option = "twitter", process = F, save = F) {
  if (option == "twitter") {
    p <- plot_time_series_twitter(df_ts_tw = df_ts_tw, process = process, save = save)
  }

  if (option == "nab") {
    p <- plot_time_series_nab(df_ts_nab = df_ts_nab, process = process, save = save)
  }

  if (option == "compare") {
    p <- plot_time_series_compare(df_ts_tw = df_ts_tw, df_ts_nab = df_ts_nab, save = save)
  }
  return(p)
}

plot_time_series_compare <- function(df_ts_tw, df_ts_nab, save = F) {
  df_ts_compare <- full_join(
    df_ts_tw %>%
      select(year, doy, date, tweet = count_sd, tweet_sm = count_sm),
    df_ts_nab %>%
      select(year, doy, date, pollen = count_mn, pollen_sm = count_sm),
    by = c("year", "doy", "date")
  ) %>%
    filter(year >= 2012) %>%
    filter(year != 2017)

  # df_spring <- data.frame(date = seq(lubridate::date("2012-01-01"),
  #   lubridate::date("2022-12-31"),
  #   by = "day"
  # )) %>%
  #   mutate(
  #     year = lubridate::year(date),
  #     month = lubridate::month(date),
  #     doy = lubridate::yday(date)
  #   ) %>%
  #   filter(year != 2017) %>%
  #   filter(month >= 2, month <= 5) %>% # Feb to May, following Anderegg et al., 2021
  #   group_by(year) %>%
  #   summarise(
  #     date_min = min(date),
  #     date_max = max(date)
  #   )
  
  df_spring <- data.frame(year = 2012:2022) %>% 
    filter(year!=2017) %>% 
    mutate(doy_min = 32,
           doy_max = 151)

  p_ts_lines <- df_ts_compare %>%
    select(-tweet, -pollen) %>%
    gather(key = "data", value = "value", -year, -doy, -date) %>%
    mutate(data = factor(data,
      levels = c("tweet_sm", "pollen_sm"),
      labels = c(
        "tweet count",
        "pollen concentration"
      )
    )) %>%
    ggplot() +
    geom_rect(
      data = df_spring,
      aes(
        xmin = doy_min + lubridate::date("2023-01-01") - 1,
        xmax = doy_max + lubridate::date("2023-01-01") - 1
      ),
      ymin = -Inf, ymax = Inf,
      fill = "gray", alpha = 0.2
    ) +
    geom_line(aes(x = doy + lubridate::date("2023-01-01") - 1, y = value, col = data, group = data)) +
    scale_color_manual(values = c("tweet count" = "dark blue", "pollen concentration" = "dark orange")) +
    scale_x_date(
      date_labels = "%b",
      breaks = seq(lubridate::date("2023-01-01"),
                                       lubridate::date("2023-12-31"),
                                       by = "3 months"
      )
    ) +
    scale_y_continuous(
      trans = scales::sqrt_trans(),
      breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2) # ,
      # labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
    ) +
    facet_wrap(. ~ year, nrow = 2, scales = "free_x") +
    labs(
      x = "Date",
      y = "Pollen phenology",
      col = "Data source"
    ) +
    theme(legend.position = "bottom")

  p_ts_corr <- ggplot(df_ts_compare) +
    geom_point(aes(x = pollen, y = tweet, col = year, group = year), alpha = 0.25) +
    geom_smooth(aes(x = pollen, y = tweet, col = year, group = year), method = "lm", se = F) +
    # theme_classic() +
    ggpubr::stat_cor(
      aes(
        x = pollen, y = tweet,
        label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")
      ),
      p.accuracy = 0.001,
      digits = 3
    ) +
    scale_color_viridis_c() +
    scale_y_continuous(
      trans = scales::sqrt_trans(),
      breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2) # ,
      # labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
    ) +
    scale_x_continuous(
      trans = scales::sqrt_trans(),
      breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2) # ,
      # labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
    ) +
    labs(
      x = "Natural pollen phenology",
      y = "Twitter pollen phenology",
      col = "Year"
    )

  if (save) {
    ggsave(
      plot = p_ts_lines,
      filename = "alldata/output/figures/supp/time_series_compare.png",
      width = 8,
      height = 8 * 0.618,
      device = png,
      type = "cairo"
    )

    ggsave(
      plot = p_ts_corr,
      filename = "alldata/output/figures/supp/time_series_corr.png",
      width = 6,
      height = 6,
      device = png,
      type = "cairo"
    )
  }
  out <- list(
    line = p_ts_lines,
    corr = p_ts_corr
  )
  return(out)
}

plot_time_series_nab <- function(df_ts_nab = df_ts_nab, cityoi = "Marietta", process = F, save = F) {
  if (!process) {
    p_ts_nab <- df_ts_nab %>%
      # filter(state=="GA") %>%
      filter(str_detect(city, cityoi)) %>%
      ggplot() +
      geom_point(aes(x = doy + lubridate::date("2023-01-01") - 1, y = count, group = year, col = year), alpha = 0.25) +
      facet_wrap(. ~ str_c(state, ", ", city), scales = "free_y") +
      scale_y_continuous(
        trans = scales::sqrt_trans(),
        breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2) # ,
        # labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
      ) +
      # theme_classic() +
      scale_color_viridis_c() +
      scale_x_date(date_labels = "%b", breaks = seq(lubridate::date("2023-01-01"),
        lubridate::date("2023-12-31"),
        by = "2 months"
      )) +
      labs(
        x = "Date",
        y = "Pollen concentration\n(grains per cubic meter of air)",
        col = "Year"
      )

    if (save) {
      ggsave(
        plot = p_ts_nab,
        filename = "alldata/output/figures/supp/time_series_nab.png",
        width = 8,
        height = 8 * 0.618,
        device = png,
        type = "cairo"
      )
    }
  }

  if (process) {
    p_ts_nab <- ggplot(df_ts_nab_proc) +
      geom_point(aes(x = date, y = count_mn), alpha = 0.1) +
      geom_line(aes(x = date, y = count_sm), col = "blue") +
      facet_wrap(. ~ year, scales = "free_x") +
      # theme_classic() +
      scale_y_continuous(
        trans = scales::sqrt_trans(),
        breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2) # ,
        # labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
      ) +
      scale_x_date(date_labels = "%b") +
      labs(
        x = "Date",
        y = "Pollen concentration (standardized)"
      )

    if (save) {
      ggsave(
        plot = p_ts_nab,
        filename = "alldata/output/figures/supp/time_series_nab_proc.png",
        width = 12,
        height = 12 * 0.618,
        device = png,
        type = "cairo"
      )
    }
  }
  return(p_ts_nab)
}

plot_time_series_twitter <- function(df_ts_tw, process = F, save = F) {
  if (!process) {
    p_ts_tw <- ggplot(df_ts_tw) +
      geom_point(aes(x = doy + lubridate::date("2023-01-01") - 1, y = count_adj, group = year, col = year), alpha = 0.25) +
      scale_y_continuous(
        trans = scales::sqrt_trans(),
        breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2) # ,
        # labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
      ) +
      # theme_classic() +
      scale_color_viridis_c() +
      scale_x_date(date_labels = "%b", breaks = seq(lubridate::date("2023-01-01"),
        lubridate::date("2023-12-31"),
        by = "2 months"
      )) +
      labs(
        x = "Date",
        y = "Tweet count",
        col = "Year"
      )

    if (save) {
      ggsave(
        plot = p_ts_tw,
        filename = "alldata/output/figures/supp/time_series_twitter.png",
        width = 8,
        height = 8 * 0.618,
        device = png,
        type = "cairo"
      )
    }
  }

  if (process) {
    p_ts_tw <- ggplot(df_ts_tw_proc %>% filter(lubridate::year(date) != 2017)) +
      geom_point(aes(x = date, y = count_sd), alpha = 0.1) +
      geom_line(aes(x = date, y = count_sm), col = "blue") +
      facet_wrap(. ~ year, scales = "free_x") +
      # theme_classic() +
      scale_y_continuous(
        trans = scales::sqrt_trans(),
        breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2) # ,
        # labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
      ) +
      scale_x_date(date_labels = "%b") +
      labs(
        x = "Date",
        y = "Tweet count (standardized)"
      )

    if (save) {
      ggsave(
        plot = p_ts_tw,
        filename = "alldata/output/figures/supp/time_series_twitter_proc.png",
        width = 12,
        height = 12 * 0.618,
        device = png,
        type = "cairo"
      )
    }
  }


  return(p_ts_tw)
}
