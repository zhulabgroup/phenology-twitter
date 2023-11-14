# select samples in three subsets
v_group <- c("pollen"   ,   "pollen-temperature", "pollen-climate")

f_sample <- str_c(.path$dat_other, "group_sample_list.rds")
if (!file.exists(f_sample)) {
  df_clean <- read_rds(.path$dat_process)

  ls_df_group_sample <- vector(mode = "list")
  for (group in v_group) {
    # detect keyword
    if (group == "pollen") {
      df_group <- df_clean
    }
    if (group == "pollen-temperature") {
      T_keywords <- c("warm", "warmer", "warming", "hot", "hotter")
      T_key_regex <- regex(paste("\\b(?i)", T_keywords, "\\b", sep = "", collapse = "|"))

      df_group <- df_clean %>%
        filter(str_detect(clean_text, T_key_regex)) # %>%
    }

    if (group == "pollen-climate") {
      CC_keywords <- c("climate", "warming")
      CC_key_regex <- regex(paste("\\b(?i)", CC_keywords, "\\b", sep = "", collapse = "|"))

      df_group <- df_clean %>%
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

    write_csv(df_group_forlabel, str_c(.path$dat_coding, group, ".csv"))
  }
  write_rds(ls_df_group_sample, f_sample)
}

# manually label

ls_df_group_labeled <- vector(mode = "list")
for (group in v_group) {
  ls_df_group_labeled[[group]] <- read_csv(str_c(.path$dat_coding, group, "_labeled.csv")) %>% as_tibble()
}

# read labeled results


ls_df_group_valid <- vector(mode = "list")
for (group in v_group) {
  if (group == "pollen") {
    ls_df_group_valid[[group]] <- read_csv(str_c(.path$dat_coding,group, "_labeled.csv")) %>%
      filter(
        pollen_phenology == 1
      )
  }
  if (group == "pollen-temperature") {
    ls_df_group_valid[[group]] <- read_csv(str_c(.path$dat_coding, group, "_labeled.csv")) %>%
      filter(
        pollen_phenology == 1,
        weather_change == 1,
        correlation == 1
      )
  }
  if (group == "pollen-climate") {
    ls_df_group_valid[[group]] <- read_csv(str_c(.path$dat_coding,group, "_labeled.csv")) %>%
      filter(
        pollen_phenology == 1,
        climate_change == 1,
        causation == 1
      )
  }
}
ls_df_group_valid
