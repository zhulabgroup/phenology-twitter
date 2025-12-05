#' @export
plot_ideology_density <- function(df_group_ideology, df_summ_group_ideology, save = F, save_path = "alldata/output/figures/") {
  df_sample_size <- df_group_ideology %>%
    group_by(group) %>%
    summarise(n = n())

  p_ideo_density <- df_group_ideology %>%
    left_join(df_sample_size, by = "group") %>%
    mutate(group = factor(group,
      levels = misc_subset(forlabel = F),
      labels = misc_subset(forlabel = T, oneline = T)
    )) %>%
    mutate(group = misc_subset_n(group, n)) %>%
    mutate(group = factor(group, levels = (.) %>% pull(group) %>% unique())) %>%
    ggplot() +
    geom_histogram(aes(x = ideology, y = after_stat(density), fill = group), color = NA, alpha = 0.5, breaks = seq(-2, 2, by = 0.5)) +
    geom_density(aes(x = ideology, col = group), fill = NA, bw = 0.2) +
    scale_color_manual(values = c("dark green", "dark blue", "dark orange")) +
    scale_fill_manual(values = c("dark green", "dark blue", "dark orange")) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_text(
      data = df_summ_group_ideology %>%
        left_join(df_sample_size, by = "group") %>%
        mutate(group = factor(group,
          levels = misc_subset(forlabel = F),
          labels = misc_subset(forlabel = T, oneline = T)
        )) %>%
        mutate(group = misc_subset_n(group, n)) %>%
        mutate(group = factor(group, levels = (.) %>% pull(group) %>% unique())) %>%
        gather(key = "lean", value = "value", -group) %>%
        mutate(value = round(value * 100)) %>%
        mutate(label = str_c(value, "%")) %>%
        mutate(x = case_when(
          lean == "left" ~ -1,
          lean == "right" ~ 1
        )),
      aes(x = x, y = 0.1, label = label)
    ) +
    labs(
      x = "(Liberal)              Ideology score    (Conservative)",
      y = "Density of user ideology",
      fill = "Group",
      col = "Group"
    ) +
    facet_wrap(. ~ group, labeller = label_parsed, ncol = 1) +
    guides(col = "none", fill = "none")

  if (save) {
    ggsave(
      plot = p_ideo_density,
      filename = str_c(save_path, "supp/ideology_density.png"),
      width = 6,
      height = 8,
      device = png,
      type = "cairo"
    )
  }

  return(p_ideo_density)
}
