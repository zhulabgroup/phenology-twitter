#' @export
read_tweet_csv <- function(path_query = "alldata/query/pollen/") {
  path_compile <- "alldata/intermediate/compiled.rds"

  v_year <- str_c(path_query, "CSV/") %>% list.files()

  for (year in v_year) {
    v_month <- seq(1, 12) %>%
      as.character() %>%
      str_pad(2, "left", "0")

    cl <- makeCluster(12)
    registerDoSNOW(cl)
    ls_df_month <-
      foreach(
        month = v_month,
        .packages = c("tidyverse", "lubridate", "qdapRegex")
      ) %dopar% {
        v_day <- list.dirs(str_c(path_query, "CSV/", year, "/", month, "/"), recursive = F) %>%
          str_sub(-2)

        ls_df_day <- vector(mode = "list")
        for (day in v_day) {
          v_files <- list.files(str_c(path_query, "CSV/", year, "/", month, "/", day, "/"), pattern = ".csv", full.names = T)
          ls_df_file <- vector(mode = "list")
          for (file in v_files) {
            df_file <- read_delim(file,
              delim = "\t",
              col_types = cols(.default = "c"),
              escape_backslash = T # escape characters like \n
            )
            if ("full_text" %in% colnames(df_file)) {
              df_file <- df_file %>%
                mutate(text = case_when(
                  !is.na(full_text) ~ full_text,
                  TRUE ~ text
                )) %>%
                select(-full_text)
            }
            if ("reply" %in% colnames(df_file)) {
              df_file <- df_file %>%
                mutate(reply = case_when(
                  !is.na(reply) ~ 1,
                  TRUE ~ 0
                ))
            }
            if ("retweet" %in% colnames(df_file)) {
              df_file <- df_file %>%
                mutate(retweet = case_when(
                  !is.na(retweet) ~ 1,
                  TRUE ~ 0
                ))
            }

            ls_df_file[[file]] <- df_file
          }
          ls_df_day[[day]] <- bind_rows(ls_df_file) %>%
            as_tibble() %>%
            mutate(day = day)
        }
        bind_rows(ls_df_day) %>%
          as_tibble() %>%
          mutate(month = month)
      }
    df_year <- bind_rows(ls_df_month)

    write_rds(df_year, str_c(path_query, "RDS/", year, ".rds"))

    stopCluster(cl)
  }

  ls_df_year <- vector(mode = "list")
  for (year in v_year) {
    ls_df_year[[year]] <- read_rds(str_c(path_query, "RDS/", year, ".rds")) %>%
      mutate(year = year)
  }
  df_compiled <- bind_rows(ls_df_year)
  write_rds(df_compiled, path_compile)

  return(path_compile)
}

#' @export
read_tweet <- function(path = "alldata/intermediate/compiled.rds", path_geo = "alldata/intermediate/geocoding.rds", geo = F) {
  df_compiled <- read_rds(path)

  if (geo) {
    df_geo <- read_rds(path_geo)
    df_compiled <- df_compiled %>%
      left_join(
        df_geo %>%
          select(user_location, state, us_state),
        by = "user_location"
      )
  }

  return(df_compiled)
}
