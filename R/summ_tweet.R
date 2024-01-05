#' @export
summ_tweet <- function(df_tweet) {
  df <- df_tweet %>%
    group_by(state) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    mutate(proportion = count / sum(count)) %>%
    mutate(percentage = round(proportion * 100), str_c("%"))

  return(df)
}
