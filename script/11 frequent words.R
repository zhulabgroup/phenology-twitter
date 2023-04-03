library(tidyverse)
df_clean <- read_rds("./data/processed/processed.rds")

library(syuzhet)
df_text_distinct <- df_clean %>%
  distinct(clean_text) %>%
  rename(text = clean_text)
nrow(df_text_distinct)

library("tm")

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

library(tidytext)
dtm_all <- text2dtm(text_all)
df_text_tidy <- tidy(dtm_all) %>%
  group_by(word = term) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  filter(word != "pollen")
# m <- as.matrix(dtm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)

# bar chart of most frequent words found in the tweets
df_text_tidy %>%
  head(15) %>%
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(x = word, y = count)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

library(wordcloud)
set.seed(1)
wordcloud(df_text_tidy$word, df_text_tidy$count,
  min.freq = 1000, scale = c(3.5, .5), random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)
