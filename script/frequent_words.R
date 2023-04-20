df_clean <- read_rds(.path$dat_process)

df_text_distinct <- df_clean %>%
  distinct(clean_text) %>%
  rename(text = clean_text)

text_all <- df_text_distinct %>% pull(text)

text_train <- text_all %>% sample(0.8 * length(text_all) %>% round())
text_valid <- setdiff(text_all, text_train)

text2dtm <- function(text) {
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("like", "just", "can", "amp"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)


  dtm <- DocumentTermMatrix(docs)

  return(dtm)
}

dtm_all <- text2dtm(text_all)
df_text_tidy <- tidy(dtm_all) %>%
  group_by(word = term) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  filter(word != "pollen") %>%
  rename(freq = count)
# m <- as.matrix(dtm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)

# bar chart of most frequent words found in the tweets
p_keyword_bar <- df_text_tidy %>%
  head(15) %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(x = word, y = freq)) +
  geom_col() +
  xlab(NULL) +
  ylab("count") +
  coord_flip() +
  theme_minimal()

if (FALSE) {
  hw <- wordcloud2(df_text_tidy %>% head(1000))
  saveWidget(hw, str_c(.path$fig_wc, "wordcloud.html"), selfcontained = F)
  webshot::webshot(str_c(.path$fig_wc, "wordcloud.html"), str_c(.path$fig_wc, "wordcloud.png"),
    vwidth = 1000, vheight = 600, delay = 10
  )
}
p_keyword_cloud <- cowplot::ggdraw() +
  cowplot::draw_image(str_c(.path$fig_wc, "wordcloud.png"))
