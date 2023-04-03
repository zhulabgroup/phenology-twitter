library(lubridate)
df_nab_ts <- nab_with_taxa_df %>%
  filter(
    date >= date("2012-01-01"),
    date <= date("2022-12-31")
  ) %>%
  filter(family == "Total") %>%
  mutate(
    doy = yday(date),
    year = year(date)
  )

ggplot(df_nab_ts) +
  geom_line(aes(x = doy, y = count^(1 / 2), group = year, col = year)) +
  facet_wrap(. ~ id, scales = "free_y") +
  theme_classic() +
  scale_color_viridis_c()

# Select for station and year with sample size
df_nab_ts %>%
  group_by(id, year) %>%
  summarise(count = n()) %>%
  pull(count) %>%
  hist()

site_year <- df_nab_ts %>%
  group_by(id, year) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count >= 100) %>%
  select(-count)

df_nab_filter <- df_nab_ts %>%
  right_join(site_year, by = c("id", "year"))
ggplot(df_nab_filter) +
  geom_line(aes(x = doy, y = count^(1 / 2), group = year, col = year)) +
  facet_wrap(. ~ id, scales = "free_y") +
  theme_classic() +
  scale_color_viridis_c()

# Standardize by station and year
site_list <- df_nab_filter$id %>% unique()
length(site_list)

library(zoo)
whitfun <- function(x, lambda) {
  max_id <- 0
  done <- F
  while (!done) {
    min_id <- min(which(!is.na(x[(max_id + 1):length(x)]))) + (max_id) # first number that is not NA
    if (min_id == Inf) { # all numbers are NA
      done <- T # consider this ts done
    } else {
      max_id <- min(which(is.na(x[min_id:length(x)]))) - 1 + (min_id - 1) # last number in the first consecutive non-NA segment
      if (max_id == Inf) {
        max_id <- length(x) # last non-NA segment is at the end of the whole ts
        done <- T # consider this ts done
      }
      x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda) # whitman smoothing for this non-NA segment
    }
  }
  return(x)
}

cl <- makeCluster(20)
registerDoSNOW(cl)
df_nab_process_list <-
  foreach(
    i = 1:nrow(site_year),
    .packages = c("tidyverse", "lubridate", "zoo")
  ) %dopar% {
    idoi <- site_year$id[i]
    yearoi <- site_year$year[i]
    df_nab_filter %>%
      filter(
        id == idoi,
        year == yearoi
      ) %>%
      select(id, count, date, doy, year) %>%
      right_join(
        data.frame(date = seq(date(paste0(yearoi, "-01-01")), date(paste0(yearoi, "-12-31")), by = "day")) %>%
          mutate(doy = yday(date)) %>%
          mutate(
            id = idoi,
            year = yearoi
          ),
        by = c("id", "year", "doy", "date")
      ) %>% # add NA to gap dates
      arrange(date) %>%
      # mutate(count_tr=count^(1/2)) %>%
      mutate(count_in = na.approx(count, date, na.rm = F, maxgap = 14)) %>%
      mutate(count_fill = replace_na(count_in, 0)) %>%
      mutate(count_sm = whitfun(count_fill, 30))
  }
df_nab_process <- bind_rows(df_nab_process_list)

# standardize
df_nab_process <- df_nab_process %>%
  mutate(count_tr = count_sm^(1 / 2)) %>%
  group_by(id) %>%
  mutate(count_sd = (count_tr - min(count_tr, na.rm = T)) / (max(count_tr, na.rm = T) - min(count_sm, na.rm = T)))

write_rds(df_nab_process, "./data/processed/nab_processed.rds")

df_nab_process <- read_rds("./data/processed/nab_processed.rds")
ggplot(df_nab_process) +
  geom_line(aes(x = doy, y = count_sd, group = year, col = year)) +
  facet_wrap(. ~ id, scales = "free_y") +
  theme_classic() +
  scale_color_viridis_c()

# Average it out
df_nab_us <- df_nab_process %>%
  group_by(year, doy, date) %>%
  summarise(count = mean(count_sd)) %>%
  rename(pollen = count)

ggplot(df_nab_us) +
  geom_line(aes(x = doy, y = pollen, group = year, col = year)) +
  theme_classic() +
  scale_color_viridis_c()

df_ts_compare <- df_tw_process %>%
  filter(type == "organic") %>%
  select(year, doy, date, tweet) %>%
  full_join(
    df_nab_us %>%
      select(year, doy, date, pollen),
    by = c("year", "doy", "date")
  )

ggplot(df_ts_compare %>%
  gather(key = "data", value = "value", -year, -doy, -date)) +
  geom_line(aes(x = doy, y = value, col = data, group = data)) +
  geom_vline(xintercept = 40) +
  geom_vline(xintercept = 180) +
  theme_classic() +
  facet_wrap(. ~ year) +
  ylab("")

ggplot(df_ts_compare %>% filter(year <= 2019)) +
  geom_point(aes(x = pollen, y = tweet, col = year, group = year), alpha = 0.5) +
  geom_smooth(aes(x = pollen, y = tweet, col = year, group = year), method = "lm", se = F) +
  theme_classic() +
  scale_color_viridis_c()

df_ts_compare_annual <- df_ts_compare %>%
  group_by(year) %>%
  filter(doy > 40, doy <= 180) %>%
  summarise(
    mean_tweet = mean(tweet, na.rm = T),
    mean_pollen = mean(pollen, na.rm = T)
  ) %>%
  filter(
    is.finite(mean_tweet),
    is.finite(mean_pollen)
  )

ggplot(df_ts_compare_annual) +
  geom_point(aes(x = mean_pollen, y = mean_tweet)) +
  geom_smooth(aes(x = mean_pollen, y = mean_tweet), method = "lm") +
  theme_classic()

cor.test(df_ts_compare_annual$mean_tweet, df_ts_compare_annual$mean_pollen)
