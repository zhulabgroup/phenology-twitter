#' @export
pred_ideo_cond_prob <- function(df_coefficient, R = 1000) {
  ls_df_pred <- vector(mode = "list", length = R)
  bin_num <- 1:8

  for (r in 1:R) {
    df_coefficient_sample <- df_coefficient %>%
      filter(sample == r)

    ls_df_pred[[r]] <- data.frame(
      bin_num = bin_num,
      pred = df_coefficient_sample$intercept + df_coefficient_sample$slope * bin_num,
      sample = r
    )
  }

  df_pred <- bind_rows(ls_df_pred) %>%
    group_by(bin_num) %>%
    summarise(
      pred_median = median(pred),
      pred_lower = quantile(pred, 0.025),
      pred_upper = quantile(pred, 0.975),
      .groups = "drop"
    )

  return(df_pred)
}
