df_ts_compare <- df_tw_ts_sd %>%
  select(year, doy, date, tweet) %>%
  full_join(
    df_nab_ts_sd %>%
      select(year, doy, date, pollen),
    by = c("year", "doy", "date")
  )

p_ts_comp <- ggplot(df_ts_compare %>%
  filter(year != 2017) %>%
  gather(key = "data", value = "value", -year, -doy, -date) %>%
  mutate(data = factor(data,
    levels = c("tweet", "pollen"),
    labels = c(
      "tweet count",
      "pollen concentration"
    )
  ))) +
  geom_line(aes(x = doy + lubridate::date("2023-01-01") - 1, y = value, col = data, group = data)) +
  geom_vline(xintercept = 40 + lubridate::date("2023-01-01") - 1) +
  geom_vline(xintercept = 180 + lubridate::date("2023-01-01") - 1) +
  scale_x_date(date_labels = "%b", breaks = seq(lubridate::date("2023-01-01"),
    lubridate::date("2023-12-31"),
    by = "3 months"
  )) +
  theme_classic() +
  facet_wrap(. ~ year, nrow = 2) +
  labs(
    x = "Date",
    y = "Standardized transformed value",
    col = "Data source"
  ) +
  theme(legend.position = "bottom")

p_ts_corr <- ggplot(df_ts_compare) +
  geom_point(aes(x = pollen, y = tweet, col = year, group = year), alpha = 0.25) +
  geom_smooth(aes(x = pollen, y = tweet, col = year, group = year), method = "lm", se = F) +
  theme_classic() +
  ggpubr::stat_cor(
    aes(
      x = pollen, y = tweet,
      label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
    ),
    digits = 3
  ) +
  scale_color_viridis_c()

# p_ts_corr_window <- ggplot(df_ts_compare %>%
#   filter(doy > 40, doy <= 180)) +
#   geom_point(aes(x = pollen, y = tweet, col = year, group = year), alpha = 0.25) +
#   geom_smooth(aes(x = pollen, y = tweet, col = year, group = year), method = "lm", se = F) +
#   theme_classic() +
#   ggpubr::stat_cor(
#     aes(
#       x = pollen, y = tweet,
#       label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
#     ),
#     digits = 3
#   ) +
#   scale_color_viridis_c()

df_ts_compare_peak <- df_ts_compare %>%
  group_by(year) %>%
  filter(doy > 40, doy <= 180) %>%
  summarise(
    # tweet = mean(tweet, na.rm = T),
    # pollen = mean(pollen, na.rm = T)
    tweet = quantile(tweet, 1, na.rm = T),
    pollen = quantile(pollen, 1, na.rm = T)
  ) %>%
  filter(
    is.finite(tweet),
    is.finite(pollen)
  )

p_ts_peak_corr <- ggplot(df_ts_compare_peak) +
  geom_point(aes(x = tweet, y = pollen)) +
  ggrepel::geom_label_repel(aes(x = tweet, y = pollen, label = year)) +
  geom_smooth(aes(x = tweet, y = pollen), method = "lm", se = F) +
  ggpubr::stat_cor(
    aes(
      x = tweet, y = pollen,
      label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
    ),
    digits = 3
  ) +
  theme_classic() +
  labs(
    x = "Peak of Twitter pollen phenology curve",
    y = "Peak of natural pollen phenology curve"
  )

# v_year <- 2012:2022 %>% setdiff(2017)
# ls_df_interannual <- vector(mode = "list")
# for (y in v_year) {
#   df_ts_subset <- df_ts_compare %>%
#     filter(year == y) %>%
#     filter(doy > 40, doy <= 180)
# 
#   df_tw_ts <- df_ts_subset %>%
#     select(doy, tweet) %>%
#     drop_na(tweet)
#   tweet_peak <- pracma::findpeaks(df_tw_ts$tweet, minpeakheight = 0.5, npeaks = 1)
# 
#   df_nab_ts <- df_ts_subset %>%
#     select(doy, pollen) %>%
#     drop_na(pollen)
#   pollen_peak <- pracma::findpeaks(df_nab_ts$pollen, minpeakheight = 0.5, npeaks = 1)
# 
#   ls_df_interannual[[y]] <- bind_rows(
#     df_tw_ts %>%
#       slice(tweet_peak[2]) %>%
#       rename(pos = doy, peak = tweet) %>%
#       gather(key = "metric") %>%
#       mutate(data = "tweet"),
#     df_nab_ts %>%
#       slice(pollen_peak[2]) %>%
#       rename(pos = doy, peak = pollen) %>%
#       gather(key = "metric") %>%
#       mutate(data = "pollen")
#   ) %>%
#     mutate(year = y)
# }
# df_interannual <- bind_rows(ls_df_interannual) %>%
#   spread(key = "data", value = "value")
# 
# p_ts_peak_corr <- ggplot(df_interannual %>% filter(metric == "pos")) +
#   geom_point(aes(x = tweet, y = pollen)) +
#   ggrepel::geom_label_repel(aes(x = tweet, y = pollen, label = year)) +
#   geom_smooth(aes(x = tweet, y = pollen), method = "lm", se = F) +
#   ggpubr::stat_cor(
#     aes(
#       x = tweet, y = pollen,
#       label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
#     ),
#     digits = 3
#   ) +
#   theme_classic() +
#   labs(
#     x = "Peak of Twitter pollen phenology curve",
#     y = "Peak of natural pollen phenology curve"
#   )
# 
# MASS::rlm(pollen ~ tweet, data = df_ts_compare_annual)
