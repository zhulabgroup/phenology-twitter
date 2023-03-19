library(tidyverse)
df_clean<-read_rds("./data/processed/processed.rds")

df_text_distinct<-df_clean %>% 
  distinct(clean_text, .keep_all = T) 
# nrow(df_text_distinct)

# climate change

# CC_keywords<-read_lines(paste0("./script/keywords/CC.txt"))
CC_keywords<-c("climate", "warming")
CC_key_regex<-regex(paste( "\\b(?i)",CC_keywords,"\\b",sep="", collapse = "|"))

df_CC<-df_text_distinct %>% 
  filter(str_detect(clean_text, CC_key_regex)) #%>% 
  # filter(!str_detect(text, "Study|study")) #%>%
  # filter(!str_detect(text, "Nothing to sneeze at"))
# paste(nrow(df_CC), "out of", nrow(df_text_distinct))
# df_CC %>% sample_n(min(10, nrow(.))) %>% select(user_screen_name,text)

df_CC_forlabel<-df_CC %>% 
  select(user_screen_name,user_description ,text, clean_text, type) %>% 
  mutate(pollen_phenology="",
         climate_change="",
         belief="",
         sentiment="",
         causation="",
         direction="",
         science="")
# write_csv(df_CC_forlabel, "./output/pollen_CC_coding.csv")
df_CC_label <- read_csv( "./output/pollen_CC_coding_labeled_03122023.csv") %>% as_tibble()

df_CC_label_full<-df_clean %>% 
  select(user=user_screen_name, text, clean_text, type) %>% 
  inner_join(df_CC_label %>% 
               select(clean_text, pollen_phenology, climate_change, causation, belief, sentiment, science),
             by="clean_text")

# weather
T_keywords<-c("weather","temperature","warm", "warming","hot", "hotter","spring")
T_key_regex<-regex(paste( "\\b(?i)",T_keywords,"\\b",sep="", collapse = "|"))

df_T<-df_text_distinct %>% 
  filter(str_detect(clean_text, T_key_regex)) #%>% 

df_T_sample<-  %>% 
  sample_n(1000)

df_T_forlabel<-df_T %>% 
  select(user_screen_name,user_description ,text, clean_text, type) %>% 
  mutate(pollen_phenology="",
         temperature_change="",
         belief="",
         sentiment="",
         causation="",
         direction="",
         science="")
write_csv(df_T_forlabel, "./output/pollen_T_coding.csv")
df_T_label <- read_csv( "./output/pollen_T_coding_labeled_03122023.csv") %>% as_tibble()

df_T_label_full<-df_clean %>% 
  select(user=user_screen_name, text, clean_text, type) %>% 
  inner_join(df_CC_label %>% 
               select(clean_text, pollen_phenology, climate_change, causation, belief, sentiment, science),
             by="clean_text")

# df_aller<-df_compiled %>% 
#   filter(str_detect(text, "allerg|hayfever|hay fever|rhinitis"))
# paste(nrow(df_aller), "out of", nrow(df_compiled))
# df_aller$text %>% head(20)
