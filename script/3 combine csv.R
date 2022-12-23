library(tidyverse)
# # library(devtools)
# # install_github("trinker/qdapRegex")
# unloadNamespace('textclean')
# library(qdapRegex)
library(doSNOW)
library(parallel)
library(lubridate)

category = "pollen"

years_list<-c(2012,2013,2014,2015,2016,2018,2019,2020,2021,2022) %>% as.character()

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
                   if ("reply" %in% colnames(df_day)) {
                     df_day<-df_day %>% 
                       mutate(reply=case_when(!is.na(reply)~1,
                                             TRUE~0)) 
                   }
                   if ("retweet" %in% colnames(df_day)) {
                     df_day<-df_day %>% 
                       mutate(retweet=case_when(!is.na(retweet)~1,
                                              TRUE~0)) 
                   }
                   
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
  df<-bind_rows(df_allmonths)#%>% 
    # arrange(month, day) %>% 
    # distinct(text, .keep_all = T)
  
  write_rds(df,paste0("./data/query/",category, "/","RDS/",year,".rds") )
  
  stopCluster(cl)
}


df_compiled_list<-vector(mode="list")
for (year in years_list) {
  df_compiled_list[[year]]<-read_rds(paste0("./data/query/",category, "/","RDS/",year,".rds")) %>% 
    mutate(year=year)
}
df_compiled<-bind_rows(df_compiled_list)
write_rds(df_compiled,paste0("./data/processed/compiled.rds") )

nrow(df_compiled)




