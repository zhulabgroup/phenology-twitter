library(tidyverse)
df_clean<-read_rds("./data/processed/processed.rds")

group_list <- c ("pollen", "pollen-weather", "pollen-climate")

df_group_sample_list <- vector(mode = "list")
for (group in group_list) {
  # detect keyword
  if (group == "pollen") {
    df_group <- df_clean
  } 
  if (group == "pollen-weather") {
    T_keywords<-c("weather", "temperature", "warm", "warmer","warming", "hot", "hotter", "spring")
    T_key_regex<-regex(paste( "\\b(?i)",T_keywords,"\\b",sep="", collapse = "|"))
    
    df_group<-df_clean %>% 
      filter(str_detect(clean_text, T_key_regex)) #%>% 
  }
  
  if (group == "pollen-climate") {
    CC_keywords<-c("climate", "warming")
    CC_key_regex<-regex(paste( "\\b(?i)",CC_keywords,"\\b",sep="", collapse = "|"))
    
    df_group<-df_clean %>% 
      filter(str_detect(clean_text, CC_key_regex))
  }
  
  # sample for labeling and user ideology scoring
  set.seed(1)
  df_group_sample <- df_group %>% 
    sample_n (min(n(), 1000))
  
  df_group_sample_list[[group]]<- df_group_sample
  # write csv for labeling distinct tweets
  df_group_distinct <- df_group_sample %>% 
    distinct(clean_text, .keep_all = T)
  
  if (group == "pollen") {
    df_group_forlabel<-df_group_distinct %>% 
      select(user_screen_name,user_description ,text, clean_text, type) %>% 
      mutate(pollen_phenology="",
             sentiment="",
             science="")
  }
  if (group == "pollen-weather") {
    df_group_forlabel<-df_group_distinct %>% 
      select(user_screen_name,user_description ,text, clean_text, type) %>% 
      mutate(pollen_phenology="",
             weather_change="",
             causation="",
             agreement="",
             sentiment="",
             science="")
  }
  if (group == "pollen-climate") {
    df_group_forlabel<-df_group_distinct %>% 
      select(user_screen_name,user_description ,text, clean_text, type) %>% 
      mutate(pollen_phenology="",
             climate_change="",
             causation="",
             agreement="",
             sentiment="",
             science="")
  }
  
  write_csv(df_group_forlabel, str_c("./data/processed/coding/", group,".csv"))
}
write_rds(df_group_sample_list, "data/processed/group_sample_list.rds")

# manually label

df_group_labeled_list<-vector(mode = "list") 
for (group in group_list) {
  df_group_labeled<- read_csv(str_c("./data/processed/coding/", group,"_labeled.csv")) %>% as_tibble()
  
}

# df_CC_label_full<-df_clean %>% 
#   select(user=user_screen_name, text, clean_text, type) %>% 
#   inner_join(df_CC_label %>% 
#                select(clean_text, pollen_phenology, climate_change, causation, belief, sentiment, science),
#              by="clean_text")
# 
# 
# df_T_label_full<-df_clean %>% 
#   select(user=user_screen_name, text, clean_text, type) %>% 
#   inner_join(df_CC_label %>% 
#                select(clean_text, pollen_phenology, climate_change, causation, belief, sentiment, science),
#              by="clean_text")
