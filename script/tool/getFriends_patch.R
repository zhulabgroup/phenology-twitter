getFriends_new<-function (screen_name = NULL, oauth, cursor = -1, user_id = NULL, 
          verbose = TRUE, sleep = 60) 
{
  my_oauth <- tweetscores:::getOAuth(oauth, verbose = verbose)
  limit <- tweetscores:::getLimitFriends(my_oauth)
  if (verbose) {
    message("start: ", limit, " API calls left")
  }

  url <- "https://api.twitter.com/1.1/friends/ids.json"
  friends <- c()
  while (cursor != 0) {
    
    if (!is.null(screen_name)) {
      params <- list(screen_name = screen_name, cursor = cursor, 
                     stringify_ids = "true")
    }
    if (!is.null(user_id)) {
      params <- list(user_id = user_id, cursor = cursor, 
                     stringify_ids = "true")
    }
    query <- lapply(params, function(x) URLencode(as.character(x)))
    url.data <- httr::GET(url, query = query, httr::config(token = my_oauth))
    
    while (url.data$status_code==429) { # rate limit exceeded
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
    message("user " ,screen_name, ": ", length(friends), " friends. Next cursor: ", cursor)

    limit <- tweetscores:::getLimitFriends(my_oauth)
    if (verbose) {
      message("continued: ", limit, " API calls left")
    }
  }
  return(friends)
}
