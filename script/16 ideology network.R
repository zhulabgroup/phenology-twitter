library(network)
library(sna)
library(tidyverse)
library(ggnetwork)

df_CC_net <- df_CC_ideo %>%
  filter(
    pollen_phenology == 1,
    climate_change == 1
  ) %>%
  filter(
    !is.na(ideology),
    ideology != 999,
    is.finite(ideology)
  ) %>%
  filter(causation == 1) %>%
  select(user, text, type, ideology) %>%
  mutate(mention = str_extract_all(text, "@\\w+")) %>%
  unnest_longer(mention) %>%
  mutate(mention = str_replace(mention, "@", "")) %>%
  group_by(to = user, from = mention) %>%
  summarise(n = n())

user_list_add <- df_CC_net %>%
  filter(!from %in% user_list_CC) %>%
  pull(from) %>%
  unique()

# df_ideology_add<-get_ideo(user_list = user_list_add)
# write_rds(df_ideology_add, "./output/ideology_add.rds")

df_ideology_full <- bind_rows(
  read_rds("./output/ideology.rds") %>% as_tibble(),
  read_rds("./output/ideology_add.rds") %>% as_tibble()
) %>%
  select(-ideology) %>%
  rename(ideology = ideology2) %>%
  filter(
    ideology != 999,
    !is.na(ideology),
    is.finite(ideology)
  )

source_list <- df_CC_net %>%
  group_by(from) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  pull(from)

df_CC_net_sel <- filter(df_CC_net) %>%
  filter(from %in% source_list) %>%
  select(from, to)

CC_net <- as.network(df_CC_net_sel, directed = TRUE, loops = T)
# network.edgecount(CC_net)
# ggnetwork(CC_net, layout = "fruchtermanreingold", cell.jitter = 0.75)
# ggnetwork(CC_net, layout = "target", niter = 100)

CC_net %v% "source" <- data.frame(user = CC_net %v% "vertex.names") %>%
  mutate(source = user %in% df_CC_net$from)
CC_net %v% "ideology" <- data.frame(user = CC_net %v% "vertex.names") %>%
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

p_net <- ggplot(CC_net) +
  geom_edges(aes(
    x = x, y = y, xend = xend, yend = yend, # ,linewidth=sqrt(n)
  ), color = "grey50", alpha = 1, arrow = arrow(length = unit(0.2, "lines"))) +
  geom_nodes(aes(x, y,
    color = ideology
  ), size = 3, alpha = 1) +
  geom_nodetext_repel(aes(x, y, label = vertex.names),
    data = function(x) {
      x[x$source == T, ]
    }
  ) +
  # geom_nodelabel(aes(x,y,label = vertex.names)) +
  scale_color_gradient2(low = "blue", high = "red", mid = "antiquewhite") +
  guides(size = "none") +
  theme_blank()

# regression

df_CC_net_ideo <- df_CC_net %>%
  left_join(df_ideology_full %>% select(to = user, to_ideo = ideology), by = "to") %>%
  left_join(df_ideology_full %>% select(from = user, from_ideo = ideology), by = "from") %>%
  drop_na()
p_ideo_reg <- ggplot(df_CC_net_ideo, aes(x = from_ideo, y = to_ideo)) +
  ggrepel::geom_label_repel(
    data = df_CC_net_ideo %>% filter(from %in% source_list) %>% group_by(from) %>% sample_n(1) %>% ungroup(),
    aes(label = from, color = from_ideo)
  ) +
  scale_color_gradient2(low = "blue", high = "red", mid = "antiquewhite") +
  guides(color = "none") +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(
    x = "source ideology",
    y = "target ideology"
  )
pearson_res <- cor.test(df_CC_net_ideo$from_ideo, df_CC_net_ideo$to_ideo)
spearman_res <- cor.test(df_CC_net_ideo$from_ideo, df_CC_net_ideo$to_ideo, method = "spearman")
