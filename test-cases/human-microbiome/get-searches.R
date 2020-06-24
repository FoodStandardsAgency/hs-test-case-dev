
# SCRIPT TO RETRIEVE ARTICLES FROM DATABASE SEARCHES AND SAVE NEW ONES TO ACADEMIC DATABASE

library(purrr)
library(magrittr)
library(dplyr)


# DOIs already in database

existingDOIs <- read_csv("etl/data/academic.csv") %>%
  mutate(doi = tolower(doi)) %>%
  pull(doi)

# Carry out PLoS search

# get last search date

lastsearch <- read_csv("data/scrapelog.csv") %>%
  filter(file == "plos") %>%
  slice(n()) %>%
  pull(date) %>%
  as.character()

# search for terms in articles added since last search
# filter out anything already in database

plos <- map_df(c("microbiome", "gut microbiome", "gut flora",
                 "gut microbes", "human gastrointestinal microbiota",
                 "gut metagenome", "gut protozoa",
                 "faecal transplantation"), plosSearch, since = lastsearch) %>%
  unique() %>%
  filter(!doi %in% existingDOIs)

# append new articles to the database

read_csv("etl/data/academic.csv", col_types = "ccccccccccccccc") %>%
  bind_rows(., plos) %>%
  write_excel_csv(., "etl/data/academia.csv")

# add entry to scrape log

newscrape <- tibble(file = "plos", date = as.character(Sys.Date()))
write_csv(newscrape, "etl/data/scrapelog.csv", append = TRUE)

