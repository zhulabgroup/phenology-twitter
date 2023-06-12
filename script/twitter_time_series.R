df_tweet <- read_rds(.path$dat_process)

# correct for user increase
df_user_num <- read_csv(str_c(.path$dat_other, "user_increase.csv")) %>%
  mutate(time = year + quarter * 0.25)
# fit quadratic plateau model if wanted
# https://gradcylinder.org/post/quad-plateau/

p_user_num <- ggplot(df_user_num) +
  geom_point(aes(x = time, y = user)) +
  theme_classic()

df_user_num_sum <- df_user_num %>%
  filter(year >= 2012) %>%
  group_by(year) %>%
  summarise(user = mean(user)) %>%
  complete(year = 2012:2022, fill = list(user = 68)) %>%
  mutate(user = user / 68)

# get time series
# about time zone https://zacharyst.com/2017/04/05/assigning-the-correct-time-to-a-tweet/
df_tw_ts <- df_tweet %>%
  # mutate(time=paste(created_at %>% substr(5,10),created_at %>% substr(27,30),created_at %>% substr(12,19)) %>%
  #          parse_date_time("BdY HMS")
  # ) %>%
  # mutate(date = lubridate::date(time)) %>%
  mutate(date = lubridate::date(paste(year, month, day, sep = "-"))) %>%
  mutate(doy = lubridate::yday(date)) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year, doy, date) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  # spread(key = "type", value = "count", fill = 0) %>%
  # gather(key = "type", value = "count", -year, -doy, -date) %>%
  # mutate(year = as.integer(year)) %>%
  right_join(
    data.frame(date = seq(lubridate::date("2012-01-01"), lubridate::date("2022-12-31"), by = "day")) %>%
      mutate(
        doy = lubridate::yday(date),
        year = lubridate::year(date)
      ),
    by = c("year", "doy", "date")
  ) %>% # add NA to gap dates
  left_join(df_user_num_sum, by = "year") %>%
  mutate(count_adj = count / user) %>%
  select(-user)


p_tw_ts <- ggplot(df_tw_ts) +
  geom_point(aes(x = doy + lubridate::date("2023-01-01") - 1, y = count_adj, group = year, col = year), alpha = 0.25) +
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2),
    labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
  ) +
  theme_classic() +
  scale_color_viridis_c() +
  scale_x_date(date_labels = "%b", breaks = seq(lubridate::date("2023-01-01"),
    lubridate::date("2023-12-31"),
    by = "2 months"
  )) +
  labs(
    x = "Date",
    y = "Tweet count",
    col = "Year"
  )

df_ts_sum <- bind_rows(
  df_tw_ts %>%
    summarise(
      sum = sum(count_adj, na.rm = T),
      mean = mean(count_adj, na.rm = T)
    ) %>%
    mutate(window = "all"),
  df_tw_ts %>%
    filter(doy > 40 & doy <= 180) %>%
    summarise(
      sum = sum(count_adj, na.rm = T),
      mean = mean(count_adj, na.rm = T)
    ) %>%
    mutate(window = "spring")
)

peak_range <- df_tw_ts %>%
  group_by(year) %>%
  arrange(desc(count_adj)) %>%
  slice(1) %>%
  drop_na(count_adj) %>%
  arrange(desc(count_adj))

highest_peak <- df_tw_ts %>%
  arrange(desc(count_adj)) %>%
  slice(1)
# df_tw_ts %>%
#   arrange(desc(count)) %>%
#   slice(1)

# "2019-08-06"
# "2013-12-10"
df_tweet %>%
  mutate(date = lubridate::date(paste(year, month, day, sep = "-"))) %>%
  filter(date == lubridate::date("2013-12-10")) %>%
  sample_n(20) %>%
  pull(text)

# process
source("script/func_whit.R")

df_tw_ts_fs <- df_tw_ts %>%
  mutate(count_in = zoo::na.approx(count, date, na.rm = F, maxgap = 14)) %>%
  mutate(count_sm = whitfun(count_in, 30)) %>%
  ungroup()

p_tw_ts_fs <- ggplot(df_tw_ts_fs) +
  geom_line(aes(x = doy, y = count_sm, group = year, col = year)) +
  theme_classic() +
  scale_color_viridis_c() +
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    breaks = scales::trans_breaks(function(x) x^(1 / 2), function(x) x^2),
    labels = scales::trans_format(function(x) x^(1 / 2), scales::math_format(.x^2))
  ) +
  ylab("tweet count")


df_tw_ts_sd <- df_tw_ts %>%
  mutate(count_tr = count_adj^(1 / 2)) %>%
  mutate(count_sd = count_tr / quantile(count_tr, 0.95, na.rm = T)) %>%
  mutate(count_in = zoo::na.approx(count_sd, date, na.rm = F, maxgap = 14)) %>%
  mutate(count_sm = whitfun(count_in, 30)) %>%
  mutate(tweet = count_sm)
