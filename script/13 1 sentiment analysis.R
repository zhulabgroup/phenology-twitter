
library(doSNOW)
library(parallel)

cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

df_text_topic <- df_text_class %>% filter(topic == 3)

df_sentiment_list <-
  foreach(
    i = 1:nrow(df_text_topic),
    .packages = c("syuzhet")
  ) %dopar% {
    text <- df_text_topic$text[i]
    ew_sentiment <- get_nrc_sentiment(text)
    print(i)
    data.frame(text, ew_sentiment)
  }

df_sentiment <- bind_rows(df_sentiment_list)

write_rds(df_sentiment, "./data/processed/sentiment.rds")

ggplot(data = df_sentiment %>%
  gather(key = "sentiment", value = "score", -text) %>%
  group_by(sentiment) %>%
  summarise(score = sum(score))) +
  geom_bar(aes(x = sentiment, y = score, fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  theme_classic()

ggplot(data = df_sentiment %>%
  # filter(!str_detect(text, "bee")) %>%
  mutate(sentiment = case_when(
    positive > negative ~ "positive",
    positive < negative ~ "negative",
    positive == negative & positive != 0 ~ "conflicted",
    positive == negative & positive == 0 ~ "indifferent"
  )) %>%
  select(text, sentiment) %>%
  group_by(sentiment) %>%
  summarise(count = n())) +
  geom_bar(aes(x = sentiment, y = count, fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  theme_classic()

df_sentiment %>%
  # filter(!str_detect(text, "bee")) %>%
  mutate(sentiment = case_when(
    positive > negative ~ "positive",
    positive < negative ~ "negative",
    positive == negative & positive != 0 ~ "conflicted",
    positive == negative & positive == 0 ~ "indifferent"
  )) %>%
  select(text, sentiment) %>%
  filter(sentiment == "indifferent") %>%
  sample_n(20)
