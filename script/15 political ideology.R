library(tidyverse)
df_clean <- read_rds("./data/processed/processed.rds")

# political ideology
my_oauth <- list(
  consumer_key = "REMOVED",
  consumer_secret = "REMOVED",
  access_token = "REMOVED",
  access_token_secret = "REMOVED"
)

# load package
# toInstall <- c("ggplot2", "scales", "R2WinBUGS", "devtools", "yaml", "httr", "RJSONIO")
# install.packages(toInstall, repos = "http://cran.r-project.org")
# library(devtools)
# install_github("pablobarbera/twitter_ideology/pkg/tweetscores")
library(tweetscores)

source("script/tool/getFriends_patch.R")
get_ideo <- function(user_list) {
  ideology_list <- vector(mode = "list")
  for (i in 1:length(user_list)) {
    user <- user_list[i]

    # downloading friends of a user
    out1 <- tryCatch(
      {
        friends <- getFriends_new(screen_name = user, oauth = my_oauth, sleep = 60)
      },
      error = function(e) {
        return(numeric(0))
      }
    )

    # estimate ideology
    if (length(out1) == 0) { # if friend ids are not available
      ideology_list[[user]] <- data.frame(user = user, ideology = NA, friends = NA)
    } else {
      # estimation using MCMC takes too long, not implemented

      # estimation using MLE
      out2 <- tryCatch(
        {
          set.seed(1)
          suppressMessages(score1 <- estimateIdeology(user, friends, method = "MLE") %>% summary() %>% `[`(2, 1))
        },
        error = function(e) {
          return(999)
        }
      )
      if (out2 == 999) {
        score1 <- 999
      }

      # estimation using correspondence analysis
      out3 <- tryCatch(
        {
          set.seed(1)
          suppressMessages(score2 <- estimateIdeology2(user, friends))
        },
        error = function(e) {
          return(999)
        }
      )
      if (out3 == 999) {
        score2 <- 999
      }

      ideology_list[[user]] <- data.frame(user = user, ideology = score1, ideology2 = score2, friends = paste(friends, collapse = ","))
    }
    print(i)
    Sys.sleep(60)
  }
  df_ideology <- bind_rows(ideology_list) %>%
    as_tibble()
  return(df_ideology)
}

df_group_sample_list <- read_rds("data/processed/group_sample_list.rds")
for (group in group_list) {
  users <- df_group_sample_list[[group]] %>%
    pull(user_screen_name) %>%
    unique() %>%
    sort()

  df_ideology <- get_ideo(user_list = users)
  write_rds(df_ideology, str_c("./data/processed/ideology/", group, ".rds"))
}


# analysis

ls_df_group_sample <- read_rds("data/processed/group_sample_list.rds")

ls_df_group <- vector(mode = "list")
for (group in v_group) {
  df_group_coding <- read_csv(str_c("./data/processed/coding/", group, "_labeled.csv"))
  if (group == "pollen") {
    df_group_valid <- df_group_coding %>%
      filter(pollen_phenology == 1)
  }
  if (group == "pollen-weather") {
    df_group_valid <- df_group_coding %>%
      filter(
        pollen_phenology == 1,
        weather_change == 1,
        correlation == 1
      )
  }
  if (group == "pollen-climate") {
    df_group_valid <- df_group_coding %>%
      filter(
        pollen_phenology == 1,
        climate_change == 1,
        causation == 1
      )
  }

  df_group_ideo <- read_rds(str_c("./data/processed/ideology/", group, ".rds"))

  df_group <- ls_df_group_sample[[group]] %>%
    select(user = user_screen_name, clean_text) %>%
    right_join(df_group_valid %>% select(clean_text),
      by = "clean_text"
    ) %>%
    select(-clean_text) %>%
    distinct(user) %>%
    left_join(
      df_group_ideo %>%
        select(user, ideology = ideology2),
      by = "user"
    ) %>%
    mutate(group = group)

  ls_df_group[[group]] <- df_group
}
df_ideo <- bind_rows(ls_df_group) %>%
  filter(ideology != 999) %>%
  filter(!is.na(ideology)) %>%
  filter(is.finite(ideology)) %>%
  mutate(group = factor(group, levels = v_group))

df_ideo_summ <- df_ideo %>%
  mutate(lean = case_when(
    ideology <= 0 ~ "left",
    TRUE ~ "right"
  )) %>%
  group_by(group, lean) %>%
  summarise(n = n()) %>%
  spread(key = "lean", value = "n") %>%
  mutate(total = left + right) %>%
  mutate(
    left = left / total,
    right = right / total
  )
df_ideo_summ

p_ideo_density <- ggplot(df_ideo) +
  geom_density(aes(x = ideology, fill = group, col = group), alpha = 0.5, bw = 0.2) +
  theme_classic() +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  xlab("ideology score") +
  ylab("frequency density")
p_ideo_density

df_ideo_bin <- df_ideo %>%
  mutate(bin = cut(ideology, breaks = seq(-2, 2, by = 0.5))) %>%
  group_by(group, bin) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  drop_na(bin)
bin_level <- df_ideo_bin$bin %>%
  unique() %>%
  levels()
df_ideo_freq_ratio <- df_ideo_bin %>%
  spread(key = "bin", value = "n") %>%
  gather(key = "bin", value = "n", -group) %>%
  mutate(n = replace_na(n, 0)) %>%
  group_by(group) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  spread(key = "group", value = "freq") %>%
  mutate(
    `from pollen to climate` = `pollen-climate` / pollen,
    `from to pollen to weather` = `pollen-weather` / pollen,
    `from weather to climate` = `pollen-climate` / `pollen-weather`
  ) %>%
  select(-pollen, -`pollen-climate`, -`pollen-weather`) %>%
  gather(key = "group", value = "ratio", -bin) %>%
  mutate(bin = factor(bin, levels = bin_level)) %>%
  mutate(group = factor(group,
    levels = c(
      "from to pollen to weather",
      "from weather to climate",
      "from pollen to climate"
    )
  ))

p_ideo_ratio <- ggplot(df_ideo_freq_ratio) +
  geom_point(aes(x = bin, y = ratio, group = group, col = group)) +
  geom_smooth(aes(x = bin, y = ratio, group = group, col = group), method = "lm", se = F) +
  ggpubr::stat_cor(
    aes(
      x = bin, y = ratio, group = group,
      col = group,
      label = paste(after_stat(r.label), after_stat(p.label), sep = "*`,`~")
    ),
    p.accuracy = 0.05,
    label.x.npc = 0.5,
    label.y.npc = "top",
    show.legend = F
  ) +
  theme_classic() +
  facet_wrap(. ~ group) +
  guides(col = "none") +
  xlab("ideology score") +
  ylab("ratio between frequency density curves")
p_ideo_ratio

### analysis

compare_ideo_dist <- function(df_CC_ideo, belief_sel, science_sel) {
  df_CC_ideo_sel <- df_CC_ideo %>%
    filter(belief %in% belief_sel) %>%
    filter(science %in% science_sel) %>%
    distinct(user, .keep_all = T)
  n <- nrow(df_CC_ideo_sel)

  df_combine <- bind_rows(
    df_CC_ideo_sel %>% select(ideology) %>% mutate(group = "climate-pollen"),
    df_rand_ideo_sel %>% select(ideology) %>% mutate(group = "pollen")
  )
  p_density <- ggplot(df_combine) +
    geom_density(aes(ideology, fill = group), alpha = 0.5, col = NA, bw = 0.2) +
    # scale_color_manual(values = c("climate-pollen" = "darkorange", "pollen" = "burlywood")) +
    scale_fill_manual(values = c("climate-pollen" = "darkorange", "pollen" = "burlywood")) +
    theme_classic()

  cross_tab <- bind_rows(
    df_CC_ideo_sel %>%
      mutate(ideology = case_when(
        ideology <= 0 ~ "left",
        TRUE ~ "right"
      )) %>%
      group_by(ideology) %>%
      summarise(n = n()) %>%
      mutate(proportion = n / sum(n)) %>%
      select(-n) %>%
      mutate(group = "climate-pollen group"),
    df_rand_ideo_sel %>%
      mutate(ideology = case_when(
        ideology <= 0 ~ "left",
        TRUE ~ "right"
      )) %>%
      group_by(ideology) %>%
      summarise(n = n()) %>%
      mutate(proportion = n / sum(n)) %>%
      select(-n) %>%
      mutate(group = "pollen group")
  ) %>%
    spread(key = "group", value = "proportion")

  ks_res <- ks.test(df_CC_ideo_sel$ideology, df_rand_ideo_sel$ideology)

  return(list(n = n, p_density = p_density, cross_tab = cross_tab, ks_res = ks_res))
}

compare_ideo_dist(df_CC_ideo, belief_sel = 1, science_sel = c(0, 1))
compare_ideo_dist(df_CC_ideo, belief_sel = 0, science_sel = c(0, 1))
compare_ideo_dist(df_CC_ideo, belief_sel = 1, science_sel = c(0))
