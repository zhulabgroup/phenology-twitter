#' @export
plot_ideo_cond_prob <- function(df_ideo_cond_prob, option = "climate", save = F) {
  p <- ggplot(df_ideo_cond_prob %>% filter(str_detect(group, option))) +
    geom_point(aes(x = bin, y = cond_prob, group = group, col = group)) +
    geom_smooth(aes(x = bin, y = cond_prob, group = group, col = group), method = "lm", se = F) +
    ggpubr::stat_cor(
      aes(
        x = bin, y = cond_prob, group = group, col = group,
        label = paste(after_stat(r.label), after_stat(p.label), sep = "*`,`~")
      ),
      p.accuracy = 0.001,
      digits = 3,
      label.x.npc = 0.2,
      label.y.npc = "top",
      # digits = 3,
      # show.legend = F
    ) +
    scale_color_manual(values = c(
      "temperature | pollen" = "dark blue",
      "climate | pollen" = "dark orange"
    )) +
    theme_classic() +
    labs(
      x = "Bin of ideology score",
      y = str_c("Conditional probability\nof attribution\nto ", option, " change"),
      col = "Group"
    ) +
    guides(col = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (save) {
    ggsave(
      plot = p,
      filename = str_c("alldata/output/figures/supp/ideology_cond_prob_", option, ".png"),
      width = 6,
      height = 6,
      device = png,
      type = "cairo"
    )
  }

  return(p)
}
