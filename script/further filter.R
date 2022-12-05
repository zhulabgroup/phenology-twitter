# climate change

CC_keywords<-read_lines(paste0("./script/keywords/CC.txt"))
CC_key_regex<-regex(paste( "\\b(?i)",CC_keywords,"\\b",sep="", collapse = "|"))

df_CC<-df_compiled %>% 
  filter(str_detect(text, CC_key_regex)) #%>% 
  # filter(!str_detect(text, "Study|study")) #%>%
  # filter(!str_detect(text, "Nothing to sneeze at"))
paste(nrow(df_CC), "out of", nrow(df_compiled))
df_CC %>% sample_n(min(50, nrow(.))) %>% mutate(user_text=paste(screen_name, text))%>% pull(user_text)

df_CC_test<-df_CC %>% 
  mutate(text=str_replace(text,"\n", " ")) %>% 
  sample_n(100) %>% 
  pull(text)

write_lines(df_CC_test, "./output/CC_test.txt")

df_aller<-df_compiled %>% 
  filter(str_detect(text, "allerg|hayfever|hay fever|rhinitis"))
paste(nrow(df_aller), "out of", nrow(df_compiled))
df_aller$text %>% head(20)
