# Load
library("tm") # for text mining
library("SnowballC") # for text stemming
library("wordcloud") # word-cloud generator 

text<-df_compiled$full_text %>% sample_n(1000)
text<-str_c(text)
docs <- Corpus(VectorSource(text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "https://t.co/")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "b'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "RT")
docs <- tm_map(docs, toSpace, "\\n")

# clean text
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

# dtm <- TermDocumentMatrix(docs)
# freq_words<-findFreqTerms(dtm, lowfreq = 20)
# # all_words<-findFreqTerms(dtm, lowfreq = 0)
# keepOnlyWords <- content_transformer(function(x, words) {
#   regmatches(x,
#              gregexpr(paste0("\\b(",  paste(words, collapse = "|"), ")\\b)"), x)
#              , invert = T) <- " "
#   x
# })
# # https://stackoverflow.com/questions/40934879/keep-exact-words-from-r-corpus
# docs_freq <- tm_map(docs, keepOnlyWords,freq_words)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v) %>% 
  filter(word!="pollen")
head(d, 10)

set.seed(1234)
pdf("")
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()