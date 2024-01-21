#' @export
plot_tweet_type <- function(df_tweet, save = F) {
  p_tweet_type <- df_tweet %>%
    group_by(type) %>%
    summarise(count = n()) %>%
    mutate(count_format = format(count, big.mark = ",")) %>%
    ungroup() %>%
    ggplot(aes(x = "", y = count, fill = type)) +
    geom_col(color = "black") +
    geom_label(aes(label = paste0(type, ": ", count_format)),
      position = position_stack(vjust = 0.5),
      col = "white"
    ) +
    scale_fill_manual(values = c("organic" = "dark blue", "reply" = "dark orange", "retweet" = "dark green")) +
    coord_polar(theta = "y", start = 0) +
    theme_void() +
    guides(fill = "none")

  if (save) {
    ggsave(
      plot = p_tweet_type,
      filename = "alldata/output/figures/supp/tweet_type.png",
      width = 6,
      height = 6,
      device = png,
      type = "cairo"
    )
  }

  return(p_tweet_type)
}
