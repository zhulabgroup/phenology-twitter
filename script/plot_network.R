

ls_p_sankey <- ls_df_sankey <- vector(mode = "list")
for (group in v_group) {
  df_net <- ls_df_group_net[[group]] %>% 
    mutate(freq = count/sum(count)) %>% 
    select(-count)
  
  df_user_type <- read_csv(str_c(.path$dat_user_type, group, "_labeled.csv"))%>% 
    select(-description) %>% 
    gather(key = "type", value = "value", -user) %>% 
    drop_na() %>% 
    select(-value) %>% 
    # mutate(type = case_when (type == "other_organization" | type == "other_individual"~"unofficial",
    #                          TRUE~type)) %>% 
    mutate(type = str_replace(type, "_", "\n")) %>% 
    mutate(type = factor(type , levels = c("media", "expert", "other\norganization","other\nindividual")))
  
  # Combine both data frames
  ls_df_sankey[[group]] <- df_net %>% 
    mutate(interaction = row_number()) %>% 
    gather(key = "end", value = "user", -interaction, -freq) %>% 
    left_join(df_user_type, by = "user") %>% 
    mutate(end = factor(end, levels = c("from", "to"), labels = c("source", "destination")))
  
  library(ggalluvial)
  ls_p_sankey[[group]] <-   ggplot( ls_df_sankey[[group]],
                aes(x = end, stratum = type, alluvium = interaction,
                    y = freq,
                    fill = type, label = type)) +
    # scale_x_discrete(expand = c(.1, .1)) +
    ggalluvial::geom_flow(alpha = 0.5) +
    ggalluvial::geom_stratum(alpha = .75, col = NA) +
    scale_x_discrete(
    expand = c(0.75, 0.75),
    ) +
    scale_fill_manual(values = c("media" = "dark orange", "expert" = "dark blue", "other\norganization"="dark green","other\nindividual"="black"))+
    scale_color_manual(values = c("media" = "dark orange", "expert" = "dark blue", "other\norganization"="dark green","other\nindividual"="black"))+
    ggrepel::geom_text_repel(
      aes(
        label = after_stat(stratum),
        hjust = ifelse(end == "source", 1, 0),
        x = as.numeric(factor(end)) + .2 * ifelse(end == "source", -1, 1),
        color = after_stat(stratum)
      ),
      stat = "stratum",
      direction = "y"
    )+
    theme_void()+
    theme(axis.text.x=element_text(angle = 0))+
    theme(strip.text = element_text(size = 12))+
    guides(fill = "none",
           color = "none")+
    ggtitle(case_when(group=="pollen"~"A. pollen",
                      group=="pollen-temperature"~"B. pollen-temperature",
                      group=="pollen-climate"~"C. pollen-climate change"))
  
  pacman::p_unload("ggalluvial")
  
}
df_sankey <- bind_rows(ls_df_sankey) %>% 
  mutate(group = factor(group, levels = v_group))

p_sankey <- ls_p_sankey[["pollen"]]+
  ls_p_sankey[["pollen-temperature"]]+
  ls_p_sankey[["pollen-climate"]]

ggsave(
  plot = p_sankey,
  filename = str_c(.path$fig, "fig-flow.png"),
  width = 12,
  height = 6,
  device = png, type = "cairo"
)

