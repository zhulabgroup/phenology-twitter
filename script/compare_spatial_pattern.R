df_state_compare <- df_tw_state %>%
  full_join(df_nab_state, by = c("state", "year")) %>%
  drop_na()

p_sp_corr <- ggplot(df_state_compare) +
  geom_point(aes(x = pollen, y = tweet, col = year, group = year), alpha = 0.5) +
  geom_smooth(aes(x = pollen, y = tweet, col = year, group = year), method = "lm", se = F) +
  ggpubr::stat_cor(aes(
    x = pollen, y = tweet,
    label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
  )) +
  theme_classic() +
  scale_color_viridis_c()

fit_sp_lm <- lm(data = df_state_compare, tweet ~ pollen) %>% summary()
fit_sp_lme <- nlme::lme(data = df_state_compare, tweet ~ pollen, random = ~ 1 | year) %>% summary()
