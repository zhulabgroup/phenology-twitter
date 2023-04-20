library(tidyverse)
# # library(devtools)
# # install_github("trinker/qdapRegex")
unloadNamespace("textclean")
library(qdapRegex)
library(doSNOW)
library(parallel)
library(lubridate)
library(reticulate)
library(lubridate)
library(zoo)
library(scales)
library(ptw)
library(nlme)
library(patchwork)
library(syuzhet)
library(tm)
library(tidytext)
library(htmlwidgets)
# install.packages("webshot")
# webshot::install_phantomjs()
library(wordcloud2)
library(topicmodels)
# toInstall <- c("ggplot2", "scales", "R2WinBUGS", "devtools", "yaml", "httr", "RJSONIO")
# install.packages(toInstall, repos = "http://cran.r-project.org")
# library(devtools)
# install_github("pablobarbera/twitter_ideology/pkg/tweetscores")
library(tweetscores)
library(network)
library(sna)
library(ggnetwork)

pacman::p_unload("all")
library(tidyverse)

category <- "pollen"

.path <- list(
  dat_query = str_c("data/query/", category, "/"),
  dat_compile = "data/processed/compiled.rds",
  dat_geo = "data/processed/geocoding.rds",
  dat_process = "data/processed/processed.rds",
  keywords = str_c("script/keywords/", category, ".txt"),
  dat_nab = "data/nab/",
  fig_st = "data/processed/stmap/",
  fig_wc = "data/processed/wordcloud/",
  dat_topic = "data/processed/topic/",
  dat_other = "data/processed/",
  dat_coding = "data/processed/coding/",
  dat_ideo = "./data/processed/ideology/"
)
