library(tidyverse)
# library(devtools)
# install_github("trinker/qdapRegex")
unloadNamespace('textclean')
library(qdapRegex)
library(doSNOW)
library(parallel)
library(lubridate)

category = "pollen"
years_list<-seq(2021,2022) %>% as.character()
for (year in years_list) {
  month_list=seq(1,12) %>% as.character() %>% str_pad(2,"left","0")
  
  cl <- makeCluster(12)
  registerDoSNOW(cl)
  df_allmonths<-
    foreach (month = month_list,
             .packages=c("tidyverse", "lubridate","qdapRegex")) %dopar% {
               day_list = list.dirs(paste0("./data/query/",category, "/","CSV/",year,"/",month, "/"), recursive = F) %>% 
                 str_sub(-2)
               
               df_alldays<-vector(mode="list")
               for (day in day_list) {
                 all_files = list.files(paste0("./data/query/",category, "/","CSV/",year,"/",month,"/", day, "/"), pattern=".csv", full.names = T)
                 df_list<-vector(mode="list")
                 for (file in all_files) {
                   df_list[[file]]<-read_delim(file, delim="\t"
                                               , escape_backslash=T # escape characters like \n
                   ) %>% 
                     mutate(full_text=
                              rm_url(full_text, pattern=pastex("@rm_twitter_url", "@rm_url"))) # https://stackoverflow.com/questions/25352448/remove-urls-from-string
                 }
                 df_alldays[[day]]<-bind_rows(df_list) %>% 
                   as_tibble() %>% 
                   mutate(day=day)
               }
               bind_rows(df_alldays) %>% 
                 as_tibble() %>% 
                 mutate(month=month)
               # df %>% head(10)
             }
  df<-bind_rows(df_allmonths)%>% 
    arrange(month, day) %>% 
    distinct(full_text, .keep_all = T)
  
  write_rds(df,paste0("./data/query/",category, "/","CSV/",year,"/","compiled.rds") )
  
  stopCluster(cl)
}


# climate change

df_CC<-df_compiled %>% 
  filter(str_detect(full_text, "climat|warming"))
paste(nrow(df_CC), "out of", nrow(df_compiled))
df_CC$full_text %>% head(20)

df_aller<-df_compiled %>% 
  filter(str_detect(full_text, "allerg|hayfever|hay fever|rhinitis"))
paste(nrow(df_aller), "out of", nrow(df_compiled))
df_aller$full_text %>% head(20)


