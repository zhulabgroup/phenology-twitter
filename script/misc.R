df_ideology <- read_rds("./output/ideology_old.rds")

# df_user_id<-data.frame(account=tweetscores::refdataCA$colname,
#                        id=tweetscores::refdataCA$id)

score1_list<-rep(NA, nrow(df_ideology))
score2_list<-rep(NA, nrow(df_ideology))
for ( i in 1:nrow(df_ideology)) {
  user<-df_ideology$user[i]
  friends <-df_ideology$friends[i]
  
  if (is.na(friends)) {
    score1<-score2<-NA
  } else {
    friends<- friends %>% str_split(pattern=",", simplify = T)
    # friends_self<-df_user_id %>% filter(account==user) %>% pull(id)
    # friends<-c(friends_self, friends)
    # estimation using MLE
    out2<-tryCatch( {
      set.seed(1)
      score1 <- estimateIdeology(user, friends, method="MLE") %>% summary() %>% `[`(2, 1)
    },
    error = function(e){ 
      return (999)
    })
    if(out2==999) {score1<-999}
    
    # estimation using correspondence analysis
    out3<-tryCatch( {
      set.seed(1)
      score2 <- estimateIdeology2(user, friends)
    },
    error = function(e){ 
      return (999)
    })
    if(out3==999) {score2<-999}  
  }
  
  score1_list[i]<-score1
  score2_list[i]<-score2
  print(i)
} 
df_ideology<-df_ideology %>% 
  mutate(ideology=score1_list,
         ideology2=score2_list) %>% 
  select(user, ideology, ideology2, friends)
write_rds(df_ideology, "./output/ideology.rds")
