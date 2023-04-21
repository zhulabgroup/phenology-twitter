f_senti <- str_c(.path$dat_other, "sentiment.rds")
if (!file.exists(f_senti)) {
  cl <- makeCluster(20, outfile = "")
  registerDoSNOW(cl)

  df_sentiment_list <-
    foreach(
      i = 1:nrow(df_text_class),
      .packages = c("tidyverse", "syuzhet")
    ) %dopar% {
      text <- df_text_class$text[i]
      ew_sentiment <- syuzhet::get_nrc_sentiment(text)
      print(i)
      df_text_class %>%
        slice(i) %>%
        mutate(ew_sentiment)
    }

  df_sentiment <- bind_rows(df_sentiment_list)

  write_rds(df_sentiment, f_senti)
} else {
  df_sentiment <- read_rds(f_senti)
}

df_senti_all <- df_sentiment %>%
  gather(key = "sentiment", value = "score", -text, -topic) %>%
  group_by(sentiment) %>%
  summarise(score = mean(score))

p_senti_all <- ggplot(data = df_senti_all %>% filter(sentiment %in% c("positive", "negative"))) +
  geom_bar(aes(x = sentiment, y = score, fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  theme_classic() +
  guides(fill = "none")

p_senti_all_fine <- ggplot(data = df_senti_all %>% filter(!sentiment %in% c("positive", "negative"))) +
  geom_bar(aes(x = sentiment, y = score, fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  theme_classic() +
  guides(fill = "none")

df_senti_topic <- df_sentiment %>%
  gather(key = "sentiment", value = "score", -text, -topic) %>%
  group_by(sentiment, topic) %>%
  # left_join(df_senti_all %>% rename(mean = score), by = "sentiment") %>%
  # mutate(score = score - mean) %>%
  summarise(score = mean(score)) %>%
  left_join(topic_names, by = "topic")

p_senti_topic <- ggplot(data = df_senti_topic %>% filter(sentiment %in% c("positive", "negative"))) +
  geom_bar(aes(x = sentiment, y = score, fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  theme_classic() +
  facet_wrap(. ~ name, ncol = 3) +
  guides(fill = "none") +
  geom_hline(yintercept = 0)

p_senti_topic_fine <- ggplot(data = df_senti_topic %>% filter(!sentiment %in% c("positive", "negative"))) +
  geom_bar(aes(x = sentiment, y = score, fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  theme_classic() +
  facet_wrap(. ~ name, ncol = 3) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 1)) +
  geom_hline(yintercept = 0)

# ggplot(data = df_sentiment %>%
#   mutate(sentiment = case_when(
#     positive > negative ~ "positive",
#     positive < negative ~ "negative",
#     positive == negative & positive != 0 ~ "conflicted",
#     positive == negative & positive == 0 ~ "indifferent"
#   )) %>%
#   select(text, sentiment) %>%
#   group_by(sentiment) %>%
#   summarise(count = n())) +
#   geom_bar(aes(x = sentiment, y = count, fill = sentiment), stat = "identity") +
#   theme(legend.position = "none") +
#   theme_classic()
