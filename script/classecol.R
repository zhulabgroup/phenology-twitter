
# nature of account
# library(devtools)
# install_github("GitTFJ/classecol")
library(classecol)
# https://github.com/GitTFJ/classecol/issues/4
load_classecol(download_models = F, download_modules = F, link_py = F)
library(addeR)
library(reticulate)
reticulate::use_python(Sys.which("python3"))
# system("python3 -m pip install --upgrade pip")
# system("python3 -m pip install pandas")
# system("python3 -m pip install numpy")
# system("python3 -m pip install nltk")
# system("python3 -m pip install bs4")
# system("python3 -m pip install scikit-learn --upgrade")
# # https://bobbyhadz.com/blog/python-no-module-named-sklearn
# # most updated version does not seem to be compatible: No module named 'sklearn.linear_model.stochastic_gradient'
# system("python3 -m pip uninstall sklearn")
# system("python3 -m pip install keras")
# system("python3 -m pip install tensorflow")
# py_run_string("import nltk")
# py_run_string("nltk.download('omw-1.4')")
# # py_run_string("from sklearn.linear_model import SGDClassifier")
system("python3 -m pip list")
# # need to restart R sometimes

py_run_string("from sklearn.linear_model.stochastic_gradient import SGDClassifier")
py_run_string("import sklearn")
py_run_string("from sklearn.linear_model import SGDClassifier")
df<-df %>% 
  mutate(class=
           bio_class(
             text_vector = paste(screen_name, description),
             type = "full")
  )

df_test = data.frame(
  text = c(
    "I love walking in nature - so serene",
    "Why are the government not stopping the destruction of the rainforest?!",
    "Tiger wins the PGA tour again!"),
  stringsAsFactors = F)
df_test$text = classecol::clean(df_test$text, level = "full")

text = df %>% 
  mutate(full_text=gsub('[[:punct:]]+', ",", full_text)) %>%
  pull(full_text) %>% 
  `Encoding<-` ("latin1" )%>% 
  textclean::replace_non_ascii()# %>% 
# classecol::clean(level = "full")
sm = as.matrix(cbind(
  valence(text),
  lang_eng(text),
  senti_matrix(text)))
nat_class(
  text_vector = text,
  senti = sm,
  type = "trimmed")