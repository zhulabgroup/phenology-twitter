library(tidyverse)
# library(devtools)
# install_github("trinker/qdapRegex")
unloadNamespace('textclean')
library(qdapRegex)
library(doSNOW)
library(parallel)
library(lubridate)

category = "pollen"

keywords<-read_lines(paste0("./script/keywords/",category, ".txt"))
key_regex<-regex(paste("\\b", keywords, "\\b", sep = "", collapse = "|"))

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
                   df_day<-read_delim(file, delim="\t"
                                      ,col_types=cols(.default = "c")
                                               , escape_backslash=T # escape characters like \n
                   ) 
                   if ("full_text" %in% colnames(df_day)) {
                     df_day<-df_day %>% 
                       mutate(text=case_when(!is.na(full_text)~full_text,
                              TRUE~text)) %>% 
                       select(-full_text)
                   }
                   df_day<-df_day %>% 
                     filter(lang=="en") %>% 
                     filter(str_detect(text, key_regex)) %>% 
                     mutate(text=
                              rm_url(text, pattern=pastex("@rm_twitter_url", "@rm_url"))) # https://stackoverflow.com/questions/25352448/remove-urls-from-string
                 df_list[[file]]<-df_day
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
    distinct(text, .keep_all = T)
  
  write_rds(df,paste0("./data/query/",category, "/","CSV/",year,"/","compiled.rds") )
  
  stopCluster(cl)
}


df_compiled_list<-vector(mode="list")
for (year in years_list) {
  df_compiled_list[[year]]<-read_rds(paste0("./data/query/",category, "/","CSV/",year,"/","compiled.rds")) %>% 
    mutate(year=year)
}
df_compiled<-bind_rows(df_compiled_list)




