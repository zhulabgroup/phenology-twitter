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
