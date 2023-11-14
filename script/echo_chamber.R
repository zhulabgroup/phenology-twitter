df_net_sample <- ls_df_group_sample[["pollen-climate"]]$sample
df_net_valid <- read_csv(str_c(.path$dat_coding, "pollen-climate", "_labeled.csv")) %>%
  filter(
    pollen_phenology == 1,
    climate_change == 1,
    causation == 1
  )
df_net_ideo <- read_rds(str_c(.path$dat_ideo, "pollen-climate", ".rds")) %>%
  select(user, ideology = ideology2)

df_net <- df_net_sample %>%
  select(user = user_screen_name, clean_text, text, type) %>%
  inner_join(df_net_valid %>% select(clean_text),
    by = "clean_text"
  ) %>%
  left_join(
    df_net_ideo,
    by = "user"
  ) %>%
  filter(ideology != 999) %>%
  filter(!is.na(ideology)) %>%
  filter(is.finite(ideology)) %>%
  mutate(mention = str_extract_all(text, "@\\w+")) %>%
  unnest_longer(mention) %>%
  mutate(mention = str_replace(mention, "@", "")) %>%
  group_by(to = user, from = mention) %>%
  summarise(n = n())

v_user_add <- df_net %>%
  filter(!from %in% df_net_ideo$user) %>%
  pull(from) %>%
  unique()

f_ideo_add <- str_c(.path$dat_ideo, "add.rds")
if (!file.exists(f_ideo_add)) {
  df_ideology_add <- get_ideo(v_user = v_user_add)
  write_rds(df_ideology_add, f_ideo_add)
} else {
  df_ideology_add <- read_rds(f_ideo_add) %>% as_tibble()
}
df_ideology_add <- df_ideology_add %>%
  select(user, ideology = ideology2)

df_ideology_full <- bind_rows(
  df_net_ideo,
  df_ideology_add
) %>%
  filter(
    ideology != 999,
    !is.na(ideology),
    is.finite(ideology)
  )

source_list <- df_net %>%
  group_by(from) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  pull(from)

df_net_sel <- filter(df_net) %>%
  filter(from %in% source_list) %>%
  select(from, to)

library(sna)
net <- network::as.network(df_net_sel, directed = TRUE, loops = T)
# ggnetwork(CC_net, layout = "fruchtermanreingold", cell.jitter = 0.75)
# ggnetwork(CC_net, layout = "target", niter = 100)

net %v% "source" <- data.frame(user = net %v% "vertex.names") %>%
  mutate(source = user %in% df_net$from)
net %v% "ideology" <- data.frame(user = net %v% "vertex.names") %>%
  left_join(
    df_ideology_full %>%
      select(user, ideology) %>%
      filter(
        ideology != 999,
        !is.na(ideology),
        is.finite(ideology)
      ) %>%
      distinct(user, .keep_all = T),
    by = "user"
  ) %>%
  pull(ideology)

p_net <- ggplot(net) +
  ggnetwork::geom_edges(aes(
    x = x, y = y, xend = xend, yend = yend, # ,linewidth=sqrt(n)
  ), color = "grey50", alpha = 1, arrow = arrow(length = unit(0.2, "lines"))) +
  ggnetwork::geom_nodes(aes(x, y,
    color = ideology
  ), size = 3, alpha = 1) +
  ggnetwork::geom_nodetext_repel(aes(x, y, label = vertex.names),
    data = function(x) {
      x[x$source == T, ]
    }
  ) +
  # geom_nodelabel(aes(x,y,label = vertex.names)) +
  scale_color_gradient2(low = "blue", high = "red", mid = "antiquewhite") +
  guides(size = "none") +
  theme_void()

pacman::p_unload("sna")

# regression
df_net_reg <- df_net %>%
  left_join(df_ideology_full %>% select(to = user, to_ideo = ideology), by = "to") %>%
  left_join(df_ideology_full %>% select(from = user, from_ideo = ideology), by = "from") %>%
  drop_na()

p_net_reg <- ggplot(df_net_reg, aes(x = from_ideo, y = to_ideo)) +
  ggrepel::geom_label_repel(
    data = df_net_reg %>% filter(from %in% source_list) %>% group_by(from) %>% sample_n(1) %>% ungroup(),
    aes(label = from, color = from_ideo)
  ) +
  scale_color_gradient2(low = "blue", high = "red", mid = "antiquewhite") +
  guides(color = "none") +
  ggpubr::stat_cor(aes(
    x = from_ideo, y = to_ideo,
    label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")
  )) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(
    x = "source ideology",
    y = "target ideology"
  )
