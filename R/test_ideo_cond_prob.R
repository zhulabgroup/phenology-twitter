#' @export
test_ideo_cond_prob <- function(df_ideo_cond_prob, option = "climate", R = 1000) {
  ls_df_coefficient <- vector(mode = "list", length = R)

  for (r in 1:R) {
    df_ideo_cond_prob_sample <- df_ideo_cond_prob %>%
      filter(str_detect(group, option)) %>%
      filter(sample == r) %>%
      arrange(bin) %>%
      mutate(bin_num = as.numeric(bin))

    model <- lm(cond_prob ~ bin_num, data = df_ideo_cond_prob_sample)
    ls_df_coefficient[[r]] <- coef(model) %>%
      t() %>%
      data.frame() %>%
      rename(intercept = `X.Intercept.`, slope = bin_num) %>%
      mutate(sample = r)
  }

  df_coefficient <- bind_rows(ls_df_coefficient)

  return(df_coefficient)
}
