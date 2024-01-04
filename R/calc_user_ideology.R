#' @export
calc_user_ideology <- function(ls_df_group_sample, path_ideo = "alldata/intermediate/ideology/") {
  myoauth <- set_twitter_api_credentials()
  for (group in misc_subset()) {
    f_ideo_group <- str_c(path_ideo, group, ".rds")

    v_user <- ls_df_group_sample[[group]]$sample %>%
      pull(user_screen_name) %>%
      unique() %>%
      sort()

    df_ideology <- calc_ideo_user(v_user = v_user, oauth = myoauth)
    write_rds(df_ideology, f_ideo_group)
  }

  return(path_ideo)
}

calc_ideo_user <- function(v_user, oauth) {
  ls_df_ideology <- vector(mode = "list")
  for (i in 1:length(v_user)) {
    user <- v_user[i]

    # downloading friends of a user
    out1 <- tryCatch(
      {
        friends <- getFriends_new(screen_name = user, oauth = oauth, sleep = 60)
      },
      error = function(e) {
        return(numeric(0))
      }
    )

    # estimate ideology
    if (length(out1) == 0) { # if friend ids are not available
      ls_df_ideology[[user]] <- data.frame(user = user, ideology = NA, friends = NA)
    } else {
      # estimation using MCMC takes too long, not implemented

      # estimation using MLE
      out2 <- tryCatch(
        {
          set.seed(1)
          suppressMessages(score1 <- tweetscores::estimateIdeology(user, friends, method = "MLE") %>% summary() %>% `[`(2, 1))
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
          suppressMessages(score2 <- tweetscores::estimateIdeology2(user, friends))
        },
        error = function(e) {
          return(999)
        }
      )
      if (out3 == 999) {
        score2 <- 999
      }

      ls_df_ideology[[user]] <- data.frame(user = user, ideology = score1, ideology2 = score2, friends = paste(friends, collapse = ","))
    }
    print(i)
    Sys.sleep(60)
  }
  df_ideology <- bind_rows(ls_df_ideology) %>%
    as_tibble()
  return(df_ideology)
}

getFriends_new <- function(screen_name = NULL, oauth, cursor = -1, user_id = NULL,
                           verbose = TRUE, sleep = 60) {
  my_oauth <- tweetscores:::getOAuth(oauth, verbose = verbose)
  limit <- tweetscores:::getLimitFriends(my_oauth)
  if (verbose) {
    message("start: ", limit, " API calls left")
  }

  url <- "https://api.twitter.com/1.1/friends/ids.json"
  friends <- c()
  while (cursor != 0) {
    if (!is.null(screen_name)) {
      params <- list(
        screen_name = screen_name, cursor = cursor,
        stringify_ids = "true"
      )
    }
    if (!is.null(user_id)) {
      params <- list(
        user_id = user_id, cursor = cursor,
        stringify_ids = "true"
      )
    }
    query <- lapply(params, function(x) URLencode(as.character(x)))
    url.data <- httr::GET(url, query = query, httr::config(token = my_oauth))

    while (url.data$status_code == 429) { # rate limit exceeded
      # message(url.data)
      Sys.sleep(sleep)
      url.data <- httr::GET(url, query = query, httr::config(token = my_oauth))
    }

    json.data <- httr::content(url.data)
    if (length(json.data$error) != 0) {
      if (verbose) {
        message(url.data)
      }
      stop("error! Last cursor: ", cursor)
    }
    friends <- c(friends, as.character(json.data$ids))
    prev_cursor <- json.data$previous_cursor_str
    cursor <- json.data$next_cursor_str
    message("user ", screen_name, ": ", length(friends), " friends. Next cursor: ", cursor)

    limit <- tweetscores:::getLimitFriends(my_oauth)
    if (verbose) {
      message("continued: ", limit, " API calls left")
    }
  }
  return(friends)
}

set_twitter_api_credentials <- function() {
  # political ideology
  my_oauth <- list(
    consumer_key = "REMOVED",
    consumer_secret = "REMOVED",
    access_token = "REMOVED",
    access_token_secret = "REMOVED"
  )

  return(my_oauth)
}
