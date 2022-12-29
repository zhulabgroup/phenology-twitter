library(tidyverse)
df_clean<-read_rds("./data/processed/processed.rds")

user_list<-df_clean %>% 
  select(user_screen_name, clean_text) %>% 
  right_join(df_CC %>% 
               select(clean_text)
             , by=c("clean_text")) %>% 
  pull(user_screen_name) %>% 
  unique()
length(user_list)
# toInstall <- c("ggplot2", "scales", "R2WinBUGS", "devtools", "yaml", "httr", "RJSONIO")
# install.packages(toInstall, repos = "http://cran.r-project.org")
# library(devtools)
# install_github("pablobarbera/twitter_ideology/pkg/tweetscores")

# political ideology
my_oauth <- list(consumer_key = "REMOVED",
                 consumer_secret = "REMOVED",
                 access_token="REMOVED",
                 access_token_secret = "REMOVED")

# load package
library(tweetscores)
# downloading friends of a user

ideology_list<-vector(mode="list")
for (i in 1:length(user_list)) {
  user=user_list[i]
  out1<-tryCatch( {
    friends <- getFriends(screen_name=user, oauth=my_oauth, sleep = 1)
  },
  error = function(e){ 
    return (numeric(0))
  })
  
  if (length(out1)==0) {
    ideology_list[[user]]<-data.frame(user=user, ideology=NA, friends=NA)
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
      ideology_list[[user]]<-data.frame(user=user, ideology=results, friends=paste(friends,  collapse=","))
    } else {
      ideology_list[[user]]<-data.frame(user=user, ideology=999, friends=paste(friends,  collapse=","))
    }
    
  }
  print(ideology_list[[user]])
  Sys.sleep(60)
}
df_ideology<-bind_rows(ideology_list)
write_rds(df_ideology, "./output/ideology.rds")

# https://github.com/twintproject/twint/issues/1346

df_ideology %>% head(15) %>% filter(ideology!=999) %>% pull(ideology) %>% median(na.rm=T)
df_ideology %>% 
  filter(ideology!=999)  %>% 
  ggplot()+
  geom_histogram(aes(ideology))+
  theme_classic()
