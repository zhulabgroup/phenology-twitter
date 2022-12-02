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