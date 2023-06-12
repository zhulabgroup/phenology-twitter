df_group_sample <- ls_df_group_sample[["pollen-climate"]]$sample
df_group_coding <- read_csv(str_c(.path$dat_coding, "pollen-climate", "_labeled.csv"))
sci_perc <- df_group_sample %>%
  select(user = user_screen_name, clean_text) %>%
  right_join(df_group_coding,
    by = "clean_text"
  ) %>%
  select(-clean_text) %>%
  filter(
    pollen_phenology == 1,
    climate_change == 1,
    causation == 1,
    agreement == 1
  ) %>%
  group_by(science) %>%
  summarise(n = n())


v_group_new <- c(v_group, "pollen-original")
ls_df_group_sample_size <- ls_df_group <- vector(mode = "list")
for (group in v_group_new) {
  if (group != "pollen-original") {
    df_group_sample <- ls_df_group_sample[[group]]$sample
    total_sample_size <- ls_df_group_sample[[group]]$sample_size
    df_group_coding <- read_csv(str_c(.path$dat_coding, group, "_labeled.csv"))
    df_group_ideo <- read_rds(str_c(.path$dat_ideo, group, ".rds")) %>%
      select(user, ideology = ideology2)
  } else {
    df_group_sample <- ls_df_group_sample[["pollen-climate"]]$sample
    total_sample_size <- ls_df_group_sample[["pollen-climate"]]$sample_size
    df_group_coding <- read_csv(str_c(.path$dat_coding, "pollen-climate", "_labeled.csv"))
    df_group_ideo <- read_rds(str_c(.path$dat_ideo, "pollen-climate", ".rds")) %>%
      select(user, ideology = ideology2)
  }

  if (group == "pollen") {
    df_group_valid <- df_group_coding %>%
      filter(pollen_phenology == 1)
  }
  if (group == "pollen-weather") {
    df_group_valid <- df_group_coding %>%
      filter(
        pollen_phenology == 1,
        weather_change == 1,
        correlation == 1
      )
  }
  if (group == "pollen-climate") {
    df_group_valid <- df_group_coding %>%
      filter(
        pollen_phenology == 1,
        climate_change == 1,
        causation == 1,
        agreement == 1
      )
  }
  if (group == "pollen-original") {
    df_group_valid <- df_group_coding %>%
      filter(
        pollen_phenology == 1,
        climate_change == 1,
        causation == 1,
        agreement == 1,
        science == 0
      )
  }

  df_group <- df_group_sample %>%
    select(user = user_screen_name, clean_text) %>%
    right_join(df_group_valid %>% select(clean_text),
      by = "clean_text"
    ) %>%
    select(-clean_text) %>%
    distinct(user) %>%
    left_join(
      df_group_ideo,
      by = "user"
    ) %>%
    mutate(group = group) %>%
    filter(ideology != 999) %>%
    filter(!is.na(ideology)) %>%
    filter(is.finite(ideology))

  ls_df_group[[group]] <- df_group

  ls_df_group_sample_size[[group]] <- bind_rows(
    group = group,
    total = total_sample_size,
    sample = df_group_sample %>% distinct(user_screen_name) %>% nrow(),
    valid = df_group_valid %>% distinct(user_screen_name) %>% nrow(),
    ideo_avail = df_group %>% distinct(user) %>% nrow()
  )
}
df_group_sample_size <- bind_rows(ls_df_group_sample_size) %>%
  mutate(rho = sample / total) %>%
  mutate(gamma = ideo_avail / valid) %>%
  mutate(total_valid = valid / rho)

# cond prob summary
cond_prob_weather <- df_group_sample_size %>%
  filter(group == "pollen-weather") %>%
  pull(total_valid) / df_group_sample_size %>%
    filter(group == "pollen") %>%
    pull(total_valid)
cond_prob_climate <- df_group_sample_size %>%
  filter(group == "pollen-climate") %>%
  pull(total_valid) / df_group_sample_size %>%
    filter(group == "pollen") %>%
    pull(total_valid)

p_venn <- cowplot::ggdraw() +
  cowplot::draw_image(str_c(.path$fig, "venn.png"))

# distribution
df_ideo <- bind_rows(ls_df_group) %>%
  mutate(group = factor(group, levels = v_group_new))

df_ideo_summ <- df_ideo %>%
  mutate(lean = case_when(
    ideology <= 0 ~ "left",
    TRUE ~ "right"
  )) %>%
  group_by(group, lean) %>%
  summarise(n = n()) %>%
  spread(key = "lean", value = "n") %>%
  mutate(total = left + right) %>%
  mutate(
    left = left / total,
    right = right / total
  ) %>%
  select(-total)

p_ideo_density <- ggplot(df_ideo %>% filter(group != "pollen-original")) +
  geom_density(aes(x = ideology, fill = group, col = group), alpha = 0.5, bw = 0.2) +
  theme_classic() +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(
    x = "(Liberal)               Ideology score           (Conservative)",
    y = "Probability density",
    fill = "Group",
    col = "Group"
  )

df_ideo_bin <- df_ideo %>%
  mutate(bin = cut(ideology, breaks = seq(-2, 2, by = 0.5))) %>%
  group_by(group, bin) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  drop_na(bin) %>%
  left_join(df_group_sample_size, by = "group") %>%
  mutate(n_est = n / gamma / rho) %>%
  select(group, bin, n = n_est)

bin_level <- df_ideo_bin$bin %>%
  unique() %>%
  levels()

df_ideo_freq_ratio <- df_ideo_bin %>%
  spread(key = "bin", value = "n") %>%
  gather(key = "bin", value = "n", -group) %>%
  mutate(n = replace_na(n, 0)) %>%
  # group_by(group) %>%
  # mutate(freq = n / sum(n)) %>%
  # ungroup() %>%
  # select(-n) %>%
  spread(key = "group", value = "n") %>%
  mutate(
    `climate | pollen` = `pollen-climate` / pollen,
    `weather | pollen` = `pollen-weather` / pollen,
    `original | pollen` = `pollen-original` / pollen,
    `climate | weather` = `pollen-climate` / `pollen-weather`,
    `original | climate` = `pollen-original` / `pollen-climate`
  ) %>%
  select(-pollen, -`pollen-climate`, -`pollen-weather`) %>%
  gather(key = "group", value = "ratio", -bin) %>%
  mutate(bin = factor(bin, levels = bin_level)) %>%
  mutate(group = factor(group,
    levels = c(
      "weather | pollen",
      "climate | pollen",
      "original | pollen",
      "climate | weather",
      "original | climate"
    )
  ))


plot_ideo_cond <- function(v_cond) {
  p <- ggplot(df_ideo_freq_ratio %>%
    filter(group %in% v_cond)) +
    geom_point(aes(x = bin, y = ratio, group = group, col = group)) +
    geom_smooth(aes(x = bin, y = ratio, group = group, col = group), method = "lm", se = F) +
    ggpubr::stat_cor(
      aes(
        x = bin, y = ratio, group = group, col = group,
        label = paste(after_stat(r.label), after_stat(p.label), sep = "*`,`~")
      ),
      label.x.npc = 0.2,
      label.y.npc = "top",
      digits = 3,
      show.legend = F
    ) +
    scale_color_manual(values = c("darkolivegreen4", "dark green")) +
    theme_classic() +
    labs(
      x = "Bin of ideology score",
      y = "Conditional probability",
      col = "Group"
    )

  return(p)
}
p_ideo_cond1 <- plot_ideo_cond(
  c(
    "weather | pollen",
    "climate | pollen" # ,
    # "original | pollen"
  )
)

p_ideo_cond2 <- plot_ideo_cond(
  c(
    "weather | pollen",
    "climate | weather",
    "original | climate"
  )
)

# # KS test
# compare_ideo_dist <- function(df_CC_ideo, belief_sel, science_sel) {
#   df_CC_ideo_sel <- df_CC_ideo %>%
#     filter(belief %in% belief_sel) %>%
#     filter(science %in% science_sel) %>%
#     distinct(user, .keep_all = T)
#   n <- nrow(df_CC_ideo_sel)
#
#   df_combine <- bind_rows(
#     df_CC_ideo_sel %>% select(ideology) %>% mutate(group = "climate-pollen"),
#     df_rand_ideo_sel %>% select(ideology) %>% mutate(group = "pollen")
#   )
#   p_density <- ggplot(df_combine) +
#     geom_density(aes(ideology, fill = group), alpha = 0.5, col = NA, bw = 0.2) +
#     # scale_color_manual(values = c("climate-pollen" = "darkorange", "pollen" = "burlywood")) +
#     scale_fill_manual(values = c("climate-pollen" = "darkorange", "pollen" = "burlywood")) +
#     theme_classic()
#
#   cross_tab <- bind_rows(
#     df_CC_ideo_sel %>%
#       mutate(ideology = case_when(
#         ideology <= 0 ~ "left",
#         TRUE ~ "right"
#       )) %>%
#       group_by(ideology) %>%
#       summarise(n = n()) %>%
#       mutate(proportion = n / sum(n)) %>%
#       select(-n) %>%
#       mutate(group = "climate-pollen group"),
#     df_rand_ideo_sel %>%
#       mutate(ideology = case_when(
#         ideology <= 0 ~ "left",
#         TRUE ~ "right"
#       )) %>%
#       group_by(ideology) %>%
#       summarise(n = n()) %>%
#       mutate(proportion = n / sum(n)) %>%
#       select(-n) %>%
#       mutate(group = "pollen group")
#   ) %>%
#     spread(key = "group", value = "proportion")
#
#   ks_res <- ks.test(df_CC_ideo_sel$ideology, df_rand_ideo_sel$ideology)
#
#   return(list(n = n, p_density = p_density, cross_tab = cross_tab, ks_res = ks_res))
# }
#
# compare_ideo_dist(df_CC_ideo, belief_sel = 1, science_sel = c(0, 1))
# compare_ideo_dist(df_CC_ideo, belief_sel = 0, science_sel = c(0, 1))
# compare_ideo_dist(df_CC_ideo, belief_sel = 1, science_sel = c(0))
