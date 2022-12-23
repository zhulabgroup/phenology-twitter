library(tidyverse)
df_compiled<-read_rds("./data/processed/compiled.rds") 
nrow(df_compiled)

#remove repeated id
df_filter <-df_compiled %>% 
  distinct(id, .keep_all=T)
nrow(df_filter)

# English
df_filter<-df_filter %>% 
  filter(lang=="en")
nrow(df_filter)

# Exact keyword
category <- "pollen"
keywords<-read_lines(paste0("./script/keywords/",category, ".txt"))
key_regex<-regex(paste("\\b(?i)", keywords, "\\b", sep = "", collapse = "|"))
df_filter<-df_filter %>%
  filter(str_detect(text, key_regex)) 
nrow(df_filter)

# within US
df_geo_all<-read_rds("./data/processed/geocoding.rds")
df_filter<-df_filter %>% 
  left_join(df_geo_all %>% select(user_location, us_state), by="user_location") %>% 
  filter(us_state=="us") %>% 
  select(-us_state)
nrow(df_filter)

## Cleaning
# library(devtools)
# install_github("trinker/qdapRegex")
unloadNamespace('textclean')
library(qdapRegex)
df_clean<-df_filter %>% 
  mutate(clean_text=str_replace_all(text,"\n", " ")) %>% 
  mutate(clean_text=
           rm_url(clean_text, pattern=pastex("@rm_twitter_url", "@rm_url"))) %>%  # https://stackoverflow.com/questions/25352448/remove-urls-from-string
  mutate(clean_text=rm_tag(clean_text, pattern="@rm_tag")) %>% 
  mutate(clean_text=str_replace_all(clean_text,regex("\\bR+T "), "")) %>% 
  mutate(clean_text=rm_non_words(clean_text,pattern = "@rm_non_words")) %>% 
  mutate(clean_text=iconv(clean_text, from="UTF-8", to="ASCII", sub="")) %>% 
  filter(str_detect(clean_text, key_regex)) # make sure all cleaned text have the "pollen" keyword
nrow(df_clean)

## type
df_type<-df_clean %>% 
  mutate(type=case_when(reply==1~"reply",
                        retweet==1~"retweet",
                        TRUE~"organic")) %>% 
  select(-reply, -retweet)

df_type %>% 
  group_by(type) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  ggplot()+
  geom_bar(aes(x="", y=count, fill=type),stat="identity", width=1)+
  coord_polar("y", start=0)+
  theme_void()
write_rds(df_type, "./data/processed/processed.rds")

