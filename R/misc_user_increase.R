#' @export
read_user_num <- function() {
  # correct for user increase
  # data source: https://www.statista.com/statistics/274564/monthly-active-twitter-users-in-the-united-states/
  path <- system.file("extdata", "user_increase.csv", package = "phenotwitter")
  df_user_num <- read_csv(path) %>%
    mutate(time = year + quarter * 0.25) %>%
    # fit quadratic plateau model if wanted
    # https://gradcylinder.org/post/quad-plateau/
    filter(year >= 2012) %>%
    group_by(year) %>%
    summarise(user = mean(user)) %>%
    mutate(type = "recorded") %>%
    complete(year = 2012:2022, fill = list(user = 68, type = "inferred")) %>%
    mutate(type = factor(type, levels = c("recorded", "inferred"))) %>%
    rename(user_num = user) %>%
    mutate(user = user_num / 68)

  return(df_user_num)
}

#' @export
plot_user_num <- function(df_user_num, save) {
  p_user_num <- ggplot(df_user_num) +
    geom_point(aes(x = year, y = user_num, fill = type), pch = 21) +
    scale_fill_manual(values = c("recorded" = "black", "inferred" = "white")) +
    geom_line(aes(x = year, y = user_num)) +
    scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
    # theme_classic() +
    labs(
      x = "Year",
      y = "US Twitter user number (million)",
      fill = ""
    ) +
    theme(legend.position = c(0.8, 0.2))

  if (save) {
    ggsave(
      plot = p_user_num,
      filename = "alldata/output/figures/supp/user_increase.png",
      width = 6,
      height = 6 * 0.618,
      device = png,
      type = "cairo"
    )
  }

  return(p_user_num)
}
