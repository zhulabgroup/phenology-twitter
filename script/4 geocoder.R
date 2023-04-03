library(tidyverse)
df_compiled <- read_rds("./data/processed/compiled.rds")

library(reticulate)

df_geo <- df_compiled %>%
  select(user_location) %>%
  distinct() %>%
  drop_na()

# search for US
us_names <- c("us", "u.s.", "u.s", "u.s.a.", "u.s.a", "usa", "united states", "united states of america", "america")
us_regex <- regex(paste("\\b(?i)", us_names, "\\b", sep = "", collapse = "|"))
df_geo_us <- df_geo %>%
  mutate(us = case_when(str_detect(user_location, us_regex) ~ "us"))

# search for city coordinate or name
source_python("~/GitHub/twitter-user-geocoder/tweet_us_city_geocoder.py")
# tug$get_city_state("xxx: (39.76838,-86.15804)")
# tug$get_city_state("Little Rock, AR")

get_city_state_func <- function(location) {
  state <- tug$get_city_state(location)$state
  if (is.null(state)) {
    state <- NA
  }
  return(state)
}

df_geo_city <- df_geo %>%
  rowwise() %>%
  mutate(city_state = get_city_state_func(user_location))

# search for state coordinate or name
source_python("~/GitHub/twitter-user-geocoder/tweet_us_state_geocoder.py")
# tug$get_state("xxx: (-37.81, 144.96)")
# tug$get_state("xxx: (39.76838,-86.15804)")
# tug$get_state("Little Rock, AR")
# tug$get_state("2121 Wyoming Ave, El Paso, TX") #problematic

get_state_func <- function(location) {
  state <- tug$get_state(location)
  if (is.null(state)) {
    state <- NA
  }
  return(state)
}

df_geo_state <- df_geo %>%
  rowwise() %>%
  mutate(state_state = get_state_func(user_location))

df_geo_all <- bind_cols(
  df_geo,
  df_geo_us %>% select(us),
  df_geo_city %>% select(city_state),
  df_geo_state %>% select(state_state)
) %>%
  mutate(state = case_when(
    !is.na(city_state) ~ city_state,
    TRUE ~ state_state
  )) %>%
  mutate(us_state = case_when(!is.na(us) | !is.na(state) ~ "us"))

df_geo_all %>%
  filter(us_state == "us") %>%
  select(user_location, state, us_state) %>%
  View()

nrow(df_geo_all)
nrow(df_geo_all %>% filter(us_state == "us"))
nrow(df_geo_all %>% filter(!is.na(state)))

write_rds(df_geo_all, "./data/processed/geocoding.rds")
