#' @export
plot_ideology_density <- function(df_group_ideology, df_summ_group_ideology, save = F) {
  # df_sample_size <- df_group_ideology %>%
  #   group_by(group) %>%
  #   summarise(n = n())

  p_ideo_density <- df_group_ideology %>%
    mutate(group = factor(group,
      levels = misc_subset(forlabel = F),
      labels = misc_subset(forlabel = T, oneline = T)
    )) %>%
    ggplot() +
    geom_density(aes(x = ideology, fill = group, col = group), alpha = 0.5, bw = 0.2) +
    scale_color_manual(values = c("dark green", "dark blue", "dark orange")) +
    scale_fill_manual(values = c("dark green", "dark blue", "dark orange")) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_text(
      data = df_summ_group_ideology %>%
        mutate(group = factor(group, labels = misc_subset(forlabel = T, oneline = T))) %>%
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
      x = "(Liberal)    Ideology score    (Conservative)",
      y = "Probability density",
      fill = "Group",
      col = "Group"
    ) +
    facet_wrap(. ~ group, ncol = 1) +
    guides(col = "none", fill = "none")

  if (save) {
    ggsave(
      plot = p_ideo_density,
      filename = "alldata/output/figures/supp/ideology_density.png",
      width = 6,
      height = 8,
      device = png,
      type = "cairo"
    )
  }

  return(p_ideo_density)
}
