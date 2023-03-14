library(lubridate)
df_tweet<-read_rds("./data/processed/processed.rds")

df_geo_all<-read_rds("./data/processed/geocoding.rds")
df_tw_st<-df_tweet %>% 
  filter(type=="organic") %>% 
  left_join(df_geo_all %>% select(user_location, state), by="user_location") %>% 
  filter(!is.na(state)) %>% 
  mutate(date=date(paste(year,month,day, sep="-"))) %>% 
  group_by(state,date) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  spread(key="state", value="count") %>% 
  mutate_if(is.numeric,~replace(., is.na(.), 0)) %>% 
  right_join(data.frame(date=seq(date("2012-01-01"),date("2022-12-31"),by="day")) 
             , by=c("date" ))  %>% # add NA to gap dates
  gather(key="state", value="count", -date) %>% 
  mutate(doy=yday(date),
         year=year(date)) %>% 
  mutate(state=toupper(state)) %>%
  mutate(state=case_when(state=="DC"~"district of columbia",
                         TRUE~state.name[match(state,state.abb)])) %>%
  left_join(data.frame(state=state.x77 %>% rownames(), population=state.x77[,"Population"]), by="state") %>% 
  mutate(state=tolower(state)) 
df_tw_st

df_tw_stmap<- map_data("state") %>% 
  full_join(df_tw_st, by=c("region"="state")) 

library(gganimate)

p_stmap <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  geom_polygon(data = df_tw_stmap , aes(x = long, y = lat, group = group, fill=count^(1/2))) +
  theme_void() +
  # geom_point(data = meta_df, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  coord_map("bonne", lat0 = 50)+
  scale_fill_viridis_c(option="magma", direction=-1, limits = c(1, 10))+
  # gganimate code
  ggtitle("{frame_time}") +
  transition_time(doy) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

# save as a video
animate(p_stmap, renderer = ffmpeg_renderer(), width = 800, height = 500)
anim_save("./output/twitter animation.mp4")
