df_ts_compare <- df_tw_ts_sd %>%
  select(year, doy, date, tweet) %>%
  full_join(
    df_nab_ts_sd %>%
      select(year, doy, date, pollen),
    by = c("year", "doy", "date")
  )

p_ts_comp <- ggplot(df_ts_compare %>%
  gather(key = "data", value = "value", -year, -doy, -date)) +
  geom_line(aes(x = doy, y = value, col = data, group = data)) +
  geom_vline(xintercept = 40) +
  geom_vline(xintercept = 180) +
  theme_classic() +
  facet_wrap(. ~ year) +
  ylab("normalized transformed count")

p_ts_corr <- ggplot(df_ts_compare) +
  geom_point(aes(x = pollen, y = tweet, col = year, group = year), alpha = 0.25) +
  geom_smooth(aes(x = pollen, y = tweet, col = year, group = year), method = "lm", se = F) +
  theme_classic() +
  ggpubr::stat_cor(aes(
    x = pollen, y = tweet,
    label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
  )) +
  scale_color_viridis_c()

df_ts_compare_annual <- df_ts_compare %>%
  group_by(year) %>%
  filter(doy > 40, doy <= 180) %>%
  summarise(
    tweet = quantile(tweet, 1, na.rm = T),
    pollen = quantile(pollen, 1, na.rm = T)
  ) %>%
  filter(
    is.finite(tweet),
    is.finite(pollen)
  )

p_ts_peak_corr <- ggplot(df_ts_compare_annual) +
  geom_point(aes(x = pollen, y = tweet)) +
  geom_smooth(aes(x = pollen, y = tweet), method = "lm") +
  ggpubr::stat_cor(aes(
    x = pollen, y = tweet,
    label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
  )) +
  theme_classic()
