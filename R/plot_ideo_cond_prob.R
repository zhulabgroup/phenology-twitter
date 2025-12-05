#' @export
plot_ideo_cond_prob <- function(df_ideo_cond_prob, option = "climate", R = 1000, save = F, save_path = "alldata/output/figures/") {
  df_summ_ideo_cond_prob <- summ_ideo_cond_prob(df_ideo_cond_prob, option = option)
  df_coefficient <- test_ideo_cond_prob(df_ideo_cond_prob, option = option, R = R)
  df_summ_coefficient <- summ_coefficient(df_coefficient, short = T)
  df_pred_ideo_cond_prob <- pred_ideo_cond_prob(df_coefficient, R = R) %>%
    left_join(df_summ_ideo_cond_prob %>% distinct(bin, bin_num), by = "bin_num")

  p <- ggplot() +
    geom_point(
      data = df_summ_ideo_cond_prob,
      aes(x = bin, y = median, col = option)
    ) +
    geom_errorbar(
      data = df_summ_ideo_cond_prob,
      aes(x = bin, ymin = lower, ymax = upper, col = option), width = 0
    ) +
    geom_line(
      data = df_pred_ideo_cond_prob,
      aes(x = bin_num, y = pred_median, col = option)
    ) +
    geom_ribbon(
      data = df_pred_ideo_cond_prob,
      aes(x = bin_num, ymin = pred_lower, ymax = pred_upper, fill = option), alpha = 0.2, col = NA
    ) +
    scale_color_manual(values = c(
      "temperature" = "dark blue",
      "climate" = "dark orange"
    )) +
    scale_fill_manual(values = c(
      "temperature" = "dark blue",
      "climate" = "dark orange"
    )) +
    ggthemes::theme_few() +
    labs(
      x = "Bin of ideology score",
      y = str_c("Conditional probability\nof attribution\nto ", option, " change"),
      col = "Group"
    ) +
    guides(
      col = "none",
      fill = "none"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate("text",
      x = 8, y =
        case_when(
          option == "temperature" ~ c(0.02, 0.9 * 0.02),
          option == "climate" ~ c(0.01, 0.9 * 0.01)
        ),
      label = c(
        sprintf(
          "italic(beta) == %s",
          df_summ_coefficient$slope_median
        ),
        sprintf(
          "`(`~%s~`,`~%s~`)`",
          df_summ_coefficient$slope_lower,
          df_summ_coefficient$slope_upper
        )
      ),
      parse = T,
      hjust = 1,
      col = case_when(
        option == "temperature" ~ "dark blue",
        option == "climate" ~ "dark orange"
      ),
      size = 3
    )

  if (save) {
    ggsave(
      plot = p,
      filename = str_c(save_path, "supp/ideology_cond_prob_", option, ".png"),
      width = 6,
      height = 6,
      device = png,
      type = "cairo"
    )
  }

  return(p)
}
