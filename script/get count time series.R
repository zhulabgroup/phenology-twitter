# get time series
# about time zone https://zacharyst.com/2017/04/05/assigning-the-correct-time-to-a-tweet/
library(lubridate)
 df_ts<-df_compiled %>% 
   mutate(time=paste(created_at %>% substr(5,10),created_at %>% substr(27,30),created_at %>% substr(12,19)) %>% 
            parse_date_time("BdY HMS")
   ) %>% 
   mutate(#hour=hour(time),
          date=date(time)) %>% 
   mutate(doy=yday(date)) %>% 
   group_by(year,doy) %>% 
   summarise(count=n()) %>% 
   ungroup() %>% 
   mutate(year=as.integer(year))
   # mutate(time=as_datetime(date)+dhours(hour)) %>% 
   # right_join(data.frame(time=seq(min(.$time),max(.$time),by="hour"))) %>% 
   # mutate(count=replace_na(count,0)) %>% 

p<-ggplot(df_ts )+
  geom_point(aes(x=doy, y=(count)^(1/4) , group=year, col=year))+
  geom_smooth(aes(x=doy, y=(count) ^(1/4), group=year, col=year), method = "loess", span=0.2)+
  theme_classic()+
  scale_color_viridis_c()
p

cairo_pdf(paste0("./output/multiyear_ts.pdf"))
p
dev.off()


df_ts %>% 
  mutate(doy=yday(date)) %>% 
  group_by(year,doy) %>% 
  summarise(count=sum(count)) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% head(6)

df_compiled %>%
  mutate(time=paste(created_at %>% substr(5,10),created_at %>% substr(27,30),created_at %>% substr(12,19)) %>% 
                        parse_date_time("BdY HMS")
) %>% 
  mutate(doy=yday(time)) %>%  filter(doy==80) %>% pull(text) %>% head(1000)

