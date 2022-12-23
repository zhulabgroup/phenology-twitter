df_tweet<-read_rds("./data/processed/processed.rds")
# get time series
# about time zone https://zacharyst.com/2017/04/05/assigning-the-correct-time-to-a-tweet/
library(lubridate)
df_tw<-df_tweet %>% 
   # mutate(time=paste(created_at %>% substr(5,10),created_at %>% substr(27,30),created_at %>% substr(12,19)) %>% 
   #          parse_date_time("BdY HMS")
   # ) %>% 
   mutate(date=date(paste(year,month,day, sep="-"))) %>% 
   mutate(doy=yday(date)) %>% 
   group_by(year,doy,date, type) %>% 
   summarise(count=n()) %>% 
   ungroup() %>% 
   spread(key="type", value = "count", fill = 0) %>% 
   gather(key="type", value = "count", -year, -doy, -date) %>% 
   mutate(year=as.integer(year)) %>% 
   right_join(data.frame(date=seq(date("2012-01-01"),date("2022-12-31"),by="day")) %>% 
                mutate(doy=yday(date),
                       year=year(date)) %>% 
                mutate(retweet=NA, reply=NA, organic=NA) %>% 
                gather(key="type", value="count", -year, -doy,-date) %>% 
                select(-count)
                , by=c("year", "doy","date" ,"type"))  # add NA to gap dates
   

p<-ggplot(df_tw %>% filter(type=="organic"))+
  geom_point(aes(x=doy, y=(count)^(1/2) , group=year, col=year), alpha=0.5)+
  # geom_smooth(aes(x=doy, y=(count) ^(1/2), group=year, col=year), method = "loess", span=0.2, se=F)+
  theme_classic()+
  scale_color_viridis_c()#+
  # facet_wrap(.~type, ncol=1)
p

cairo_pdf(paste0("./output/ts_point.pdf"))
p
dev.off()

# process
library(zoo)
whitfun <- function(x, lambda) {
  max_id <- 0
  done <- F
  while (!done) {
    min_id <- min(which(!is.na(x[(max_id + 1):length(x)]))) + (max_id) # first number that is not NA
    if (min_id == Inf) { # all numbers are NA
      done <- T # consider this ts done
    } else {
      max_id <- min(which(is.na(x[min_id:length(x)]))) - 1 + (min_id - 1) # last number in the first consecutive non-NA segment
      if (max_id == Inf) {
        max_id <- length(x) # last non-NA segment is at the end of the whole ts
        done <- T # consider this ts done
      }
      x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda) # whitman smoothing for this non-NA segment
    }
  }
  return(x)
}

df_tw_process<-df_tw %>% 
  group_by(type) %>% 
  mutate(count_in = na.approx(count, date, na.rm = F, maxgap = 14)) %>%
  # mutate(count_fill = replace_na(count_in, 0)) %>%
  mutate(count_sm = whitfun(count_in, 30)) %>% 
  mutate(count_tr=count_sm^(1/2)) %>% 
  mutate(count_sd = (count_tr - min(count_tr, na.rm = T)) / (max(count_tr, na.rm = T) - min(count_sm, na.rm = T))) %>%
  rename(tweet=count_sd) %>% 
  ungroup()


p<-ggplot(df_tw_process %>% filter(type=="organic"))+
  geom_line(aes(x=doy, y=tweet, group=year, col=year))+
  theme_classic()+
  scale_color_viridis_c()#+
# facet_wrap(.~type, ncol=1)
p

cairo_pdf(paste0("./output/ts_line.pdf"))
p
dev.off()


# # look into specific dates
# df_ts %>% 
#   filter(year=="2016") %>% 
#   group_by(year,doy) %>% 
#   summarise(count=sum(count)) %>% 
#   ungroup() %>% 
#   arrange(desc(count)) %>% head(6)
# 
# df_compiled %>%
#   filter(year=="2016") %>% 
#   mutate(time=paste(created_at %>% substr(5,10),created_at %>% substr(27,30),created_at %>% substr(12,19)) %>% 
#                         parse_date_time("BdY HMS")
# ) %>% 
#   mutate(doy=yday(time)) %>%  filter(doy==98) %>% pull(text) %>% head(1000)

