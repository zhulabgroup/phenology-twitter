library(topicmodels)
df_perplexity <- data.frame(k = 2:10, perplexity = NA)
dtm_train <- text2dtm(text_train)
dtm_valid <- text2dtm(text_valid)
for (i in 1:nrow(df_perplexity)) {
  lda <- LDA(dtm_all, k = df_perplexity$k[i], control = list(seed = 1, alpha = 0.01))
  df_perplexity$perplexity[i] <- perplexity(lda)
  print(df_perplexity[i, ])
}

df_perplexity %>%
  ggplot() +
  geom_line(aes(x = k, y = perplexity)) +
  theme_classic()

lda <- LDA(dtm_all, k = 8, control = list(seed = 1, alpha = 0.01))
topics <- tidy(lda, matrix = "beta")
topics

top_terms <- topics %>%
  filter(term != "pollen") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

gamma <- tidy(lda, matrix = "gamma")

df_text_class <- gamma %>%
  arrange(document) %>%
  group_by(document) %>%
  arrange(desc(gamma), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(document = as.integer(document)) %>%
  arrange(document) %>%
  mutate(text = text_all) %>%
  select(-document, -gamma)


df_text_class %>%
  filter(topic == 3) %>%
  sample_n(20)
