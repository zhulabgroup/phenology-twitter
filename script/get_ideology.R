# df_clean <- read_rds(.path$dat_process)

# political ideology
my_oauth <- list(
  consumer_key = "REMOVED",
  consumer_secret = "REMOVED",
  access_token = "REMOVED",
  access_token_secret = "REMOVED"
)

# load package
source("script/tool/getFriends_patch.R")
get_ideo <- function(v_user) {
  ls_df_ideology <- vector(mode = "list")
  for (i in 1:length(v_user)) {
    user <- v_user[i]

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
      ls_df_ideology[[user]] <- data.frame(user = user, ideology = NA, friends = NA)
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

      ls_df_ideology[[user]] <- data.frame(user = user, ideology = score1, ideology2 = score2, friends = paste(friends, collapse = ","))
    }
    print(i)
    Sys.sleep(60)
  }
  df_ideology <- bind_rows(ls_df_ideology) %>%
    as_tibble()
  return(df_ideology)
}

ls_df_group_sample <- read_rds(str_c(.path$dat_other, "group_sample_list.rds"))
for (group in v_group) {
  f_ideo_group <- str_c(.path$dat_ideo, group, ".rds")
  if (!file.exists(f_ideo_group)) {
    v_user <- ls_df_group_sample[[group]]$sample %>%
      pull(user_screen_name) %>%
      unique() %>%
      sort()

    df_ideology <- get_ideo(v_user = v_user)
    write_rds(df_ideology, f_ideo_group)
  }
}
