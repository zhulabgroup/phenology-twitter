f_perp <- str_c(.path$dat_topic, "perplexity.rds")
f_lda <- str_c(.path$dat_top, "lda.rds")
if (!file.exists(f_perp)) {
  # dtm_train <- text2dtm(text_train)
  #  dtm_valid <- text2dtm(text_valid)
  v_k <- 2:10
  df_perplexity <- data.frame(k = v_k, perplexity = NA)
  ls_lda <- vector(mode = "list")
  for (i in 1:nrow(df_perplexity)) {
    lda <- topicmodels::LDA(dtm_all, k = df_perplexity$k[i], control = list(seed = 1, alpha = 0.01))
    ls_lda[[i]] <- lda
    df_perplexity$perplexity[i] <- topicmodels::perplexity(lda)
    print(df_perplexity[i, ])
  }
  names(ls_lda) <- v_k
  write_rds(df_perplexity, f_perp)
  write_rds(ls_lda, f_lda)
} else {
  df_perplexity <- read_rds(f_perp)
  ls_lda <- read_rds(f_lda)
}

p_perplexity <- df_perplexity %>%
  ggplot() +
  geom_line(aes(x = k, y = perplexity)) +
  theme_classic()

lda <- ls_lda[["8"]]
topics <- tidytext::tidy(lda, matrix = "beta")

topic_names <- data.frame(
  topic = 1:8,
  name = c(
    "car and allergy",
    "air quality",
    "car",
    "forecast",
    "spring",
    "food",
    "nature",
    "allergy"
  )
) %>%
  mutate(name = factor(name, levels = name))

top_terms <- topics %>%
  filter(term != "pollen") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  left_join(topic_names, by = "topic")

p_topic <- top_terms %>%
  mutate(term = tidytext::reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, scales = "free") +
  tidytext::scale_y_reordered() +
  theme_minimal()

gamma <- tidytext::tidy(lda, matrix = "gamma")

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
