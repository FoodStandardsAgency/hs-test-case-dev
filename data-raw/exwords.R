## code to prepare `exwords` dataset goes here

library(tidyverse)
library(reticulate)

# read in exclusion words
exwords <- append(readLines("data-raw/studywords.txt"), readLines("data-raw/topicwords.txt"))
exwords <- unique(exwords[!exwords == ""])

# get lemma versions

source_python("python/python-functions.py")

exwords <- get_lemmas(exwords) %>% unlist() %>% unique()

usethis::use_data(exwords, overwrite = T)
