# climate change

df_CC<-df_compiled %>% 
  filter(str_detect(text, "climat|warming")) %>% 
  filter(!str_detect(text, "Study|study")) #%>% 
  # filter(!str_detect(text, "Nothing to sneeze at"))
paste(nrow(df_CC), "out of", nrow(df_compiled))
df_CC$text %>% head(20)

df_aller<-df_compiled %>% 
  filter(str_detect(text, "allerg|hayfever|hay fever|rhinitis"))
paste(nrow(df_aller), "out of", nrow(df_compiled))
df_aller$text %>% head(20)