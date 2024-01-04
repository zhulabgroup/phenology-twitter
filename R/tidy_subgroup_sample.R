#' @export
tidy_subgroup_sample <- function(df_tweet, path_sample = "alldata/intermediate/group_sample_list.rds", path_coding = "alldata/intermediate/coding/", writecsv = F) {
  ls_df_group_sample <- vector(mode = "list")
  for (group in misc_subset()) {
    # detect keyword
    if (group == "pollen") {
      df_group <- df_tweet
    }
    if (group == "pollen-temperature") {
      T_keywords <- c("warm", "warmer", "warming", "hot", "hotter")
      T_key_regex <- regex(paste("\\b(?i)", T_keywords, "\\b", sep = "", collapse = "|"))

      df_group <- df_tweet %>%
        filter(str_detect(clean_text, T_key_regex)) # %>%
    }

    if (group == "pollen-climate") {
      CC_keywords <- c("climate", "warming")
      CC_key_regex <- regex(paste("\\b(?i)", CC_keywords, "\\b", sep = "", collapse = "|"))

      df_group <- df_tweet %>%
        filter(str_detect(clean_text, CC_key_regex))
    }

    # sample for labeling and user ideology scoring
    set.seed(1)
    sample_size <- df_group %>%
      distinct(user_screen_name) %>%
      nrow()
    df_group_sample <- df_group %>%
      sample_n(min(n(), 1000))

    ls_df_group_sample[[group]] <- list(sample = df_group_sample, sample_size = sample_size)

    # write csv for labeling distinct tweets
    df_group_distinct <- df_group_sample %>%
      distinct(clean_text, .keep_all = T)

    if (group == "pollen") {
      df_group_forlabel <- df_group_distinct %>%
        select(user_screen_name, user_description, text, clean_text, type) %>%
        mutate(
          pollen_phenology = "",
          sentiment = "",
          science = ""
        )
    }
    if (group == "pollen-temperature") {
      df_group_forlabel <- df_group_distinct %>%
        select(user_screen_name, user_description, text, clean_text, type) %>%
        mutate(
          pollen_phenology = "",
          temperature_change = "",
          causation = "",
          agreement = "",
          sentiment = "",
          science = ""
        )
    }
    if (group == "pollen-climate") {
      df_group_forlabel <- df_group_distinct %>%
        select(user_screen_name, user_description, text, clean_text, type) %>%
        mutate(
          pollen_phenology = "",
          climate_change = "",
          causation = "",
          agreement = "",
          sentiment = "",
          science = ""
        )
    }

    if (writecsv) {
      write_csv(df_group_forlabel, str_c(path_coding, group, ".csv"))
    }
  }
  write_rds(ls_df_group_sample, path_sample)

  return(ls_df_group_sample)
}
