library(tidyverse)
df_clean <- read_rds("./data/processed/processed.rds")

set.seed(1)
user_list_rand <- df_clean %>%
  sample_n(1000) %>%
  pull(user_screen_name) %>%
  unique()
# length(user_list_rand)

user_list_CC <- df_clean %>%
  select(user_screen_name, clean_text) %>%
  right_join(
    df_CC %>%
      select(clean_text),
    by = c("clean_text")
  ) %>%
  pull(user_screen_name) %>%
  unique()
# length(user_list_CC)

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
# downloading friends of a user

source("script/tool/getFriends_patch.R")
get_ideo <- function(user_list) {
  ideology_list <- vector(mode = "list")
  for (i in 1:length(user_list)) {
    user <- user_list[i]
    out1 <- tryCatch(
      {
        friends <- getFriends_new(screen_name = user, oauth = my_oauth, sleep = 60)
      },
      error = function(e) {
        return(numeric(0))
      }
    )

    if (length(out1) == 0) {
      ideology_list[[user]] <- data.frame(user = user, ideology = NA, friends = NA)
    } else {
      # estimate ideology with MCMC method
      # results <- estimateIdeology(user, friends, method="MLE")
      # summary(results)

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

if (FALSE) {
  df_ideology_rand <- get_ideo(user_list = user_list_rand)
  write_rds(df_ideology_rand, "./output/ideology_rand.rds")

  df_ideology <- get_ideo(user_list = user_list_CC)
  write_rds(df_ideology, "./output/ideology.rds")
}

# https://github.com/twintproject/twint/issues/1346

# df_ideology %>% head(15) %>% filter(ideology!=999) %>% pull(ideology) %>% median(na.rm=T)
# df_ideology %>%
#   filter(ideology!=999)  %>%
#   ggplot()+
#   geom_histogram(aes(ideology))+
#   theme_classic()

### analysis
df_ideology <- read_rds("./output/ideology.rds") %>%
  as_tibble() %>%
  select(-ideology) %>%
  rename(ideology = ideology2)
df_CC_label <- read_csv("./output/pollen_CC_coding_labeled_03122023.csv") %>% as_tibble()

df_CC_ideo <- df_clean %>%
  select(user = user_screen_name, text, clean_text, type) %>%
  inner_join(
    df_CC_label %>%
      select(clean_text, pollen_phenology, climate_change, causation, belief, sentiment, science),
    by = "clean_text"
  ) %>%
  left_join(
    df_ideology %>%
      select(user, ideology),
    by = "user"
  ) %>%
  filter(
    pollen_phenology == 1,
    climate_change == 1
  ) %>%
  filter(
    !is.na(ideology),
    ideology != 999,
    is.finite(ideology)
  ) %>%
  filter(causation == 1)

# df_CC_ideo_sel %>%  filter(ideology>0) %>% View()

df_rand_ideo <- read_rds("./output/ideology_rand.rds") %>%
  as_tibble() %>%
  select(-ideology) %>%
  rename(ideology = ideology2)

df_rand_ideo_sel <- df_rand_ideo %>%
  filter(
    !is.na(ideology),
    ideology != 999,
    is.finite(ideology)
  )

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
    geom_density(aes(ideology, fill = group), alpha = 0.5, col=NA, bw = 0.2) +
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
