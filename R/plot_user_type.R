#' @export
plot_user_type <- function(df_tweet) {
  p_user_type <- df_tweet %>%
    group_by(type) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    ggplot() +
    geom_bar(aes(x = "", y = count, fill = type), stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void()

  return(p_user_type)
}
