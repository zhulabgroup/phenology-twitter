library(tidyverse)
# library(devtools)
# install_github("trinker/qdapRegex")
unloadNamespace('textclean')
library(qdapRegex)
library(doSNOW)
library(parallel)
library(lubridate)
# toInstall <- c("ggplot2", "scales", "R2WinBUGS", "devtools", "yaml", "httr", "RJSONIO")
# install.packages(toInstall, repos = "http://cran.r-project.org")
# library(devtools)
# install_github("pablobarbera/twitter_ideology/pkg/tweetscores")

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

df_compiled_list<-vector(mode="list")
for (year in years_list) {
  df_compiled_list[[year]]<-read_rds(paste0("./data/query/",category, "/","CSV/",year,"/","compiled.rds")) %>% 
    mutate(year=year)
}
df_compiled<-bind_rows(df_compiled_list)

# get time series
# about time zone https://zacharyst.com/2017/04/05/assigning-the-correct-time-to-a-tweet/
library(lubridate)
day_grid<-df_compiled %>% 
  distinct(year,month, day)
cl <- makeCluster(20)
registerDoSNOW(cl)
df_ts_list<-
  foreach (i = 1:nrow(day_grid),
         .packages = c("tidyverse", "lubridate")) %dopar% {
  df_ts<-df_compiled %>% 
    filter(year==day_grid$year[i],
           month==day_grid$month[i],
           day==day_grid$day[i]) %>% 
    mutate(time=paste(created_at %>% substr(5,10),created_at %>% substr(27,30),created_at %>% substr(12,19)) %>% 
             parse_date_time("BdY HMS")
    ) %>% 
    mutate(hour=hour(time),
           date=date(time)) %>% 
    group_by(date,hour) %>% 
    summarise(count=n()) %>% 
    mutate(time=as_datetime(date)+dhours(hour)) %>% 
    right_join(data.frame(time=seq(min(.$time),max(.$time),by="hour"))) %>% 
    arrange(time) %>% 
    mutate(count=replace_na(count,0)) %>% 
    mutate(#month=monthday_grid$month[i],
           #day=monthday_grid$day[i],
           hour=hour(time),
           year=year(time),
           month=month(time),
           day=day(time),
           date=date(time)
           ) 
  df_ts
}
df_ts<-bind_rows(df_ts_list)
stopCluster(cl)

ggplot(df_ts)+
  geom_point(aes(x=time, y=count))+
  theme_classic()#+
  # facet_wrap(.~month, ncol=1, scales = "free_x")

p<-ggplot(df_ts %>% 
            mutate(doy=yday(date)) %>% 
         group_by(year,doy) %>% 
         summarise(count=sum(count)) %>% 
           ungroup()
         # mutate(count=case_when(count>10~count))
         )+
  geom_point(aes(x=doy, y=count , group=year, col=year))+
  geom_smooth(aes(x=doy, y=count , group=year, col=year), method = "loess", span=0.2)+
  theme_classic()+
  scale_color_viridis_c()
p

cairo_pdf(paste0("./output/multiyear_ts.pdf"))
p
dev.off()

# climate change

df_CC<-df_compiled %>% 
  filter(str_detect(full_text, "climat|warming"))
paste(nrow(df_CC), "out of", nrow(df_compiled))
df_CC$full_text %>% head(20)

df_aller<-df_compiled %>% 
  filter(str_detect(full_text, "allerg|hayfever|hay fever|rhinitis"))
paste(nrow(df_aller), "out of", nrow(df_compiled))
df_aller$full_text %>% head(20)

# political ideology
my_oauth <- list(consumer_key = "REMOVED",
                 consumer_secret = "REMOVED",
                 access_token="REMOVED",
                 access_token_secret = "REMOVED")

# load package
library(tweetscores)
# downloading friends of a user
user_list<-df$screen_name %>% unique()
ideology_list<-vector(mode="list")
for (user in user_list[1:15]) {
  out1<-tryCatch( {
    friends <- getFriends(screen_name=user, oauth=my_oauth)
  },
  error = function(e){ 
    return (numeric(0))
  })
  
  if (length(out1)==0) {
    ideology_list[[user]]<-data.frame(user=user, ideology=NA)
  } else {
    # estimate ideology with MCMC method
    # results <- estimateIdeology(user, friends, method="MLE")
    # summary(results)
    
    # estimation using correspondence analysis
    out2<-tryCatch( {
      results <- estimateIdeology2(user, friends)
      },
    error = function(e){ 
      return (999)
    })
    
    if (out2!=999) {
      ideology_list[[user]]<-data.frame(user=user, ideology=results)
    } else {
      ideology_list[[user]]<-data.frame(user=user, ideology=999)
    }
    
  }
  # Sys.sleep(60)
}
ideology_df<-bind_rows(ideology_list)
write_csv(ideology_df, "/nfs/turbo/seas-zhukai/phenology/Twitter/ideology_test2.csv")

# https://github.com/twintproject/twint/issues/1346

ideology_df %>% head(15) %>% filter(ideology!=999) %>% pull(ideology) %>% median(na.rm=T)
ideology_df %>% head(15) %>% filter(ideology!=999) %>% pull(ideology) %>% hist()

# nature of account
# library(devtools)
# install_github("GitTFJ/classecol")
library(classecol)
# https://github.com/GitTFJ/classecol/issues/4
load_classecol(download_models = F, download_modules = F, link_py = F)
library(addeR)
library(reticulate)
reticulate::use_python(Sys.which("python3"))
# system("python3 -m pip install --upgrade pip")
# system("python3 -m pip install pandas")
# system("python3 -m pip install numpy")
# system("python3 -m pip install nltk")
# system("python3 -m pip install bs4")
system("python3 -m pip install scikit-learn==0.24.2")
# # https://bobbyhadz.com/blog/python-no-module-named-sklearn
# # most updated version does not seem to be compatible: No module named 'sklearn.linear_model.stochastic_gradient'
# system("python3 -m pip uninstall sklearn")
# system("python3 -m pip install keras")
# system("python3 -m pip install tensorflow")
# py_run_string("import nltk")
# py_run_string("nltk.download('omw-1.4')")
# # py_run_string("from sklearn.linear_model import SGDClassifier")
system("python3 -m pip list")
# # need to restart R sometimes

py_run_string("from sklearn.linear_model.stochastic_gradient import SGDClassifier")
py_run_string("import sklearn")
py_run_string("from sklearn.linear_model import SGDClassifier")
df<-df %>% 
  mutate(class=
           bio_class(
             text_vector = paste(screen_name, description),
             type = "full")
  )

df_test = data.frame(
  text = c(
    "I love walking in nature - so serene",
    "Why are the government not stopping the destruction of the rainforest?!",
    "Tiger wins the PGA tour again!"),
  stringsAsFactors = F)
df_test$text = classecol::clean(df_test$text, level = "full")

text = df %>% 
  mutate(full_text=gsub('[[:punct:]]+', ",", full_text)) %>%
  pull(full_text) %>% 
  `Encoding<-` ("latin1" )%>% 
  textclean::replace_non_ascii()# %>% 
  # classecol::clean(level = "full")
sm = as.matrix(cbind(
  valence(text),
  lang_eng(text),
  senti_matrix(text)))
nat_class(
  text_vector = text,
  senti = sm,
  type = "trimmed")
