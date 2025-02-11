#' @export
test_st_gradient <- function(df_st_metric_compare) {
  ls_df_coefficient <- vector(mode = "list")

  for (data in c("tweet", "pollen")) {
    model <- lm(reformulate("lat", response = data), data = df_st_metric_compare)

    df_coef <- coef(model) %>%
      t() %>%
      data.frame() %>%
      select(slope = lat)

    ci <- confint(model)
    df_coef <- df_coef %>%
      mutate(
        slope_lower = ci["lat", 1], # Lower bound of slope CI
        slope_upper = ci["lat", 2], # Upper bound of slope CI
      )

    ls_df_coefficient[[data]] <- df_coef %>%
      mutate(data = data)
  }

  df_coefficient <- bind_rows(ls_df_coefficient)

  return(df_coefficient)
}
