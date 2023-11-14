make_network <- function (df_sample, df_valid, group) {
  df_retweet <- df_sample %>%
    select(user = user_screen_name, clean_text, text, type) %>%
    inner_join(df_valid %>% select(clean_text),
               by = "clean_text"
    ) %>%
    mutate(retweet = str_extract_all(text, "RT @\\w+", simplify = F))
    
  df_net <- bind_rows(df_retweet %>% 
                        rowwise() %>% 
                        filter(length(retweet)==0) %>% 
                        mutate(retweet =NA) %>% 
                        ungroup() ,
                      df_retweet %>% 
                          rowwise() %>% 
                          filter(length(retweet)>0) %>% 
                          ungroup() %>% 
                          unnest_longer(retweet)
                        )%>%
    mutate(retweet = str_replace(retweet, "RT @", "")) %>%
    # select(to = user, from = mention) %>% 
    group_by(to = user, from = retweet) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    drop_na() 
  
  write_csv(df_net, str_c(.path$dat_network, group,".csv"))
  return(df_net)
}

ls_df_group_net <- vector(mode = "list")
for (group in v_group) {
  ls_df_group_net[[group]] <- make_network(df_sample = ls_df_group_sample[[group]]$sample,
                                           df_valid = ls_df_group_valid[[group]],
                                           group = group)
}
