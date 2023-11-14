df_user_desc <- df_tweet %>% 
  group_by(user = user_screen_name) %>% summarise(description = user_description[1]) %>% 
  drop_na()

code_user_type <- function(df_net, df_user_desc, group) {
  df_user <- data.frame(user = c(df_net$to, df_net$from)) %>% 
    drop_na() %>% 
    distinct() %>% 
    left_join(df_user_desc, by = "user") %>% 
    mutate(media = "",
           expert = "",
           other_organization = "",
           other_individual = "")
  
  write_csv(df_user, str_c(.path$dat_user_type, group, ".csv"))
  
  return(df_user)
}

code_user_type(df_net = df_net_pollen,
               df_user_desc = df_user_desc,
               group = "pollen")

code_user_type(df_net = df_net_climate,
               df_user_desc = df_user_desc,
               group = "pollen-climate")

code_user_type(df_net = df_net_temperature,
               df_user_desc = df_user_desc,
               group = "pollen-temperature")
