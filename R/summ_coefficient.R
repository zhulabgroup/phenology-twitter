#' @export
summ_coefficient <- function(df_coefficient, short = T) {
  df_summ <- df_coefficient %>%
    summarise(
      intercept_median = median(intercept),
      intercept_lower = quantile(intercept, 0.025),
      intercept_upper = quantile(intercept, 0.975),
      slope_median = median(slope),
      slope_lower = quantile(slope, 0.025),
      slope_upper = quantile(slope, 0.975),
      .groups = "drop"
    )

  if (short) {
    # 3 significant numbers
    df_summ <- df_summ %>%
      mutate(across(everything(), ~ signif(., 3)))
  }

  return(df_summ)
}
