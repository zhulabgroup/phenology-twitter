#' @export
tidy_tweet_clean <- function(df_compiled_geo, path_process = "alldata/intermediate/processed.rds") {
  # remove repeated id
  df_filter <- df_compiled_geo %>%
    distinct(id, .keep_all = T)

  # English
  df_filter <- df_filter %>%
    filter(lang == "en")

  # Exact keyword
  keywords <- "pollen"
  key_regex <- regex(paste("\\b(?i)", keywords, "\\b", sep = "", collapse = "|"))
  df_filter <- df_filter %>%
    filter(str_detect(text, key_regex))

  # within US
  df_filter <- df_filter %>%
    filter(us_state == "us") %>%
    select(-us_state)

  ## Cleaning
  df_clean <- df_filter %>%
    mutate(clean_text = str_replace_all(text, "\n", " ")) %>%
    mutate(
      clean_text =
        qdapRegex::rm_url(clean_text, pattern = qdapRegex::pastex("@rm_twitter_url", "@rm_url"))
    ) %>% # https://stackoverflow.com/questions/25352448/remove-urls-from-string
    mutate(clean_text = qdapRegex::rm_tag(clean_text, pattern = "@rm_tag")) %>%
    mutate(clean_text = str_replace_all(clean_text, regex("\\bR+T "), "")) %>%
    mutate(clean_text = qdapRegex::rm_non_words(clean_text, pattern = "@rm_non_words")) %>%
    mutate(clean_text = iconv(clean_text, from = "UTF-8", to = "ASCII", sub = "")) %>%
    filter(str_detect(clean_text, key_regex))

  ## type
  df_clean <- df_clean %>%
    mutate(type = case_when(
      reply == 1 ~ "reply",
      retweet == 1 ~ "retweet",
      TRUE ~ "organic"
    )) %>%
    select(-reply, -retweet)
  # nrow(df_clean)

  write_rds(df_clean, path_process)

  return(df_clean)
}
