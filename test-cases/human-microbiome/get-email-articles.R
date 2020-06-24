
# SCRIPT TO RETRIEVE NEW ARTICLES FROM EMAILS THAT HAVE BEEN SAVED TO THE EMAIL FOLDER,
# AND SAVE NEW ONES OUT TO THE ACADEMIC DATABASE

library(readr)
library(magrittr)
library(stringr)
library(dplyr)
library(purrr)
library(tibble)

# list of emails in email folder

emailfolder <- "etl/data/emails"

emails <- list.files(emailfolder)

# identify which are new since last time

lastscrape <- read_csv("data/scrapelog.csv") %>%
  filter(file == "emails") %>%
  slice(n()) %>%
  pull(date) %>%
  as.character()

newemails <- enframe(emails) %>%
  mutate(date = as.Date(str_extract(value, "^([0-9]){4}\\-([0-9]){2}\\-([0-9]){2}"))) %>%
  filter(date >= lastscrape) %>%
  pull(value)

# get text of links from these emails

linktext <- map(newemails, getLinkText, emailfolder = emailfolder) %>% unlist()

# get existing DOIs

existingDOIs <- read_csv("data/academic.csv") %>%
  mutate(doi = tolower(doi)) %>%
  pull(doi)

# emails with DOIs - get unique list not in existing and get info

dois <- map(linktext, getDOIs) %>% unlist() %>% unique() %>% .[!. %in% existingDOIs]
articles1 <- map_df(dois, fetchFromDOI)

# emails with titles - get unique list, get info, filter out DOIs in existing or above

titles <- map(linktext, getTitle) %>% unlist() %>% unique()
articles2 <- map_df(titles, insistently(fetchFromTitle)) %>%
  filter(!doi %in% existingDOIs & !doi %in% articles1$doi)

# get abstracts if absent

noabs <- bind_rows(articles1, articles2) %>%
  mutate(abstract = {if("abstract" %in% names(.)) paste0(abstract) else ""}) %>%
  filter(abstract == "" | abstract == "NA") %>%
  pull(doi)

newabs9 <- map_df(noabs[801:836], slowly(getAbstract, rate = rate_delay(pause = 1)))

# bind everything together, filter for relevant search terms

searchterms <- "Microbiome|Flora|Microbiota|metagenome|Human metagenome|gut flora|gut bacterium|gut microbes|gut protozoa|faecal transplantation"

allrelevant <- bind_rows(articles1, articles2) %>%
  mutate_at(vars(abstract), na_if, "NA") %>%
  left_join(newabs, by = "doi") %>%
  mutate(abstract = if_else(!is.na(abstract.y), abstract.y, abstract.x)) %>%
  select(-abstract.x, -abstract.y) %>%
  filter_at(., vars(title, abstract), any_vars(str_detect(., regex(searchterms, ignore_case = TRUE))))

# clean up the data to get the required fields

cleandata <- CrossRefCleanup(allrelevant)

# get any body text that is available

bodies <- map_df(cleandata$doi, slowly(getBody, rate = rate_delay(pause = 1)))

newdata <- left_join(cleandata, bodies, by = "doi")

# append the new data to the database

read_csv("etl/data/academic.csv", col_types = "ccccccccccccccc") %>%
  bind_rows(., newdata) %>%
  write_excel_csv(., "etl/data/academic.csv")

# add entry to scrape log

newscrape <- tibble(file = "emails", date = as.character(Sys.Date()))
write_csv(newscrape, "etl/data/scrapelog.csv", append = TRUE)
