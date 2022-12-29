library(lubridate)
df_tweet<-read_rds("./data/processed/processed.rds")

df_geo_all<-read_rds("./data/processed/geocoding.rds")
df_tw_state<-df_tweet %>% 
  filter(type=="organic") %>% 
  left_join(df_geo_all %>% select(user_location, state), by="user_location") %>% 
  filter(!is.na(state)) %>% 
  mutate(year=as.integer(year))%>%
  group_by(state, year) %>% 
  mutate(date=date(paste(year,month,day, sep="-"))) %>% 
  summarise(count=n(),
            days=date %>% unique() %>% length()
            ) %>% 
  ungroup() %>% 
  mutate(state=toupper(state)) %>%
  mutate(state=case_when(state=="DC"~"district of columbia",
                         TRUE~state.name[match(state,state.abb)])) %>%
  left_join(data.frame(state=state.x77 %>% rownames(), population=state.x77[,"Population"]), by="state") %>% 
  mutate(state=tolower(state)) %>%
  mutate(intensity=(count/days)^(1/2)) %>%
  # mutate(intensity=(count/days)^(1/2)/(population)) %>%
  select(state,year, intensity) %>%
  # spread(key="year", value="intensity") %>% 
  # gather(key="year", value="intensity",-state) %>% 
  arrange(desc(intensity)) %>% 
  rename(tweet=intensity)
df_tw_state

df_tw_map<- map_data("state") %>% 
  full_join(df_tw_state, by=c("region"="state")) 

p_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, linewidth = 0.2) +
  geom_polygon(data = df_tw_map, aes(x = long, y = lat, group = group, fill=tweet)) +
  theme_void() +
  facet_wrap(.~year)+
  # geom_point(data = meta_df, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  coord_map("bonne", lat0 = 50)+
  scale_fill_viridis_c(option="magma", direction=-1)
p_map


