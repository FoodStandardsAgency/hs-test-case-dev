
devtools::load_all()
library(tidyverse)
library(lubridate)

searchterms <- c("gut microbiome", "gut flora",
                 "gut microbes", "human gastrointestinal microbiota",
                 "gut metagenome", "gut protozoa",
                 "faecal transplantation")

relevancy <- "Microbiome|Flora|Microbiota|metagenome|Human metagenome|gut flora|gut bacterium|gut microbes|gut protozoa|faecal transplantation"

# EXTERNAL SEARCH RESULTS

# external csv of search results from Web of Science (assume all relevant)

wos <- read_csv("test-cases/human-microbiome/wos_all.csv") %>%
  filter(!is.na(doi))

#dois <- wos %>% pull(doi)
#bodies <- map(dois, getBody)

wos <- wos %>% 
  #left_join(bodies) %>%  
  mutate(body = "") %>% 
  mutate(pubdate = str_replace(pubdate, "SPR|SUM|WIN", "")) %>% 
  mutate(pubdate = if_else(grepl("([A-Z]){3,3}-([A-Z]){3,3}", pubdate), str_remove(pubdate, "-([A-Z]){3,3}$"), pubdate)) %>%
  mutate(pubdate = if_else(grepl("([A-Z]){3,3}", pubdate), paste0(pubdate,"-01"), pubdate)) %>%
  mutate(pubdate = as.character(ymd(paste0(PY,"-",pubdate)))) %>% 
  mutate(pubtype = doctype) %>% 
  select(-PY, -fundacknowledge, -cited, -ID, -language, -doctype) %>% 
  mutate(volume = as.character(volume)) %>% 
  cleanac() %>% 
  mutate(source = "wos") 

# PLOS SEARCH

# plos search over time horizon
# filter for relevant terms in title/abstract

plos <- map_df(searchterms, plosSearch, since = "2010-01-01") %>%
  unique() %>% 
  cleanac() %>% 
  filter_at(., vars(title, abstract), any_vars(str_detect(., regex(relevancy, ignore_case = TRUE)))) %>% 
  mutate(source = "plossearch")

# CROSSREF SEARCH

# crossref search over time horizon
# crossref returns a lot of results so need to break time period into chunks

dates <- seq.Date(as.Date("2009-01-01"), as.Date("2020-01-01"), "days")
sinces <- dates[c(T,F)]
tos <- dates[c(F,T)]

# title search only - cannot search abstracts
# multiple words are treated as "OR" - cannot look for a phrase
# only searched for one phrase ("gut microbiome"), with title relevancy filter

crsearchresult <- map2(as.character(sinces), as.character(tos), safely(crSearch, otherwise = "fail"), query = "gut microbiome")

cr <- transpose(crsearchresult) %>% 
  .$result %>% 
  .[. != "fail"] %>% 
  bind_rows() %>% 
  unique()

# improve abstract coverage by running getAbstract()

noabs <- cr %>% 
  filter(abstract == "") %>% 
  pull(doi)

abstracts <- map(noabs, slowly(safely(getAbstract,otherwise = "fail"), rate = purrr::rate_delay(pause = 0.1)))

crsearch <- transpose(abstracts) %>% 
  .$result %>% 
  .[. != "fail"] %>% 
  bind_rows() %>% 
  right_join(cr, by = "doi") %>% 
  mutate(abstract = if_else(abstract.y == "", abstract.x, abstract.y)) %>% 
  select(-abstract.x, -abstract.y) %>% 
  filter_at(., vars(title, abstract), any_vars(str_detect(., regex(relevancy, ignore_case = TRUE)))) %>%
  mutate(body = "") %>%
  mutate(author = str_to_title(author)) %>% 
  mutate(author = if_else(author == ",", "", author)) %>% 
  cleanac() %>% 
  mutate(source = "crossref") 


# EMAILS

# email monitoring

emailfolder <- "test-cases/human-microbiome/emails"

# only want to search for details if the DOI has not already been found by a previous method

wosdoi <- wos %>% mutate(doi = tolower(doi)) %>% pull(doi)
plosdoi <- plos %>% mutate(doi = tolower(doi)) %>% pull(doi)
crdoi <- crsearch %>% mutate(doi = tolower(doi)) %>% pull(doi)
existingDOIs <- c(wosdoi, plosdoi, crdoi) %>% unique()

# find as many articles as possible from the titles/DOIs that can be pulled from the emails

allarticles <- getEmailArticles(emailfolder, existingDOIs)

allarticles <- read_csv("test-cases/human-microbiome/emails.csv")

# get missing abstracts

noabs <- allarticles %>%
  filter(abstract == "") %>% 
  pull(doi)

abstracts <- map(noabs, slowly(safely(getAbstract,otherwise = "fail"), rate = purrr::rate_delay(pause = 0.1)))

emails <- transpose(abstracts) %>% 
  .$result %>% 
  .[. != "fail"] %>% 
  bind_rows() %>% 
  right_join(allarticles, by = "doi") %>% 
  mutate(abstract = if_else(is.na(abstract.y), abstract.x, abstract.y)) %>% 
  select(-abstract.x, -abstract.y) %>% 
  
  # filter for relevancy
  
  filter_at(., vars(title, abstract), any_vars(str_detect(., regex(relevancy, ignore_case = TRUE)))) %>% 
  
  filter(!doi %in% existingDOIs) %>% 
  mutate(body = "") %>% 
  cleanac() %>% 
  mutate(source = "email") 


# full dataset

plos2 <- plos %>% filter(!doi %in% wosdoi)
crsearch2 <- crsearch %>% filter(!doi %in% c(wosdoi, plosdoi))

microbiome <- wos %>% 
  bind_rows(plos2) %>% 
  bind_rows(crsearch2) %>% 
  bind_rows(emails) %>% 
  mutate(published = as.character(published)) %>% 
  replace(., is.na(.), "") %>% 
  mutate(published = ymd(published))


# cleaned up 'affiliation' column

mbaff <- microbiome %>% 
  select(doi, affiliation) %>% 
  filter(affiliation != "") %>% 
  mutate(affiliation = iconv(affiliation, "utf-8", "ASCII//TRANSLIT")) %>% 
  mutate(affiliation = str_remove_all(affiliation, fixed("?"))) %>%
  mutate(affiliation = str_remove_all(affiliation, fixed("\""))) %>% 
  mutate(affiliation = str_replace_all(affiliation, fixed(" & "), " and ")) %>% 
  mutate(affiliation = str_replace_all(affiliation, fixed("Univ "), "University of ")) %>% 
  mutate(affiliation = str_replace_all(affiliation, fixed("Univ,"), "University")) %>%
  mutate(affiliation = str_replace_all(affiliation, fixed("  "), " "))

uilist <- readRDS("data-raw/uilist.rds")

uimatch <- map_df(uilist$institution, function(x) mbaff %>% 
                    mutate(whichinst = str_extract(affiliation, fixed(x, ignore_case = T))) %>% 
                    filter(!is.na(whichinst))) %>% 
  select(doi, affiliation, whichinst) %>% 
  arrange(doi)

affcol <- uimatch %>% 
  filter(whichinst != "-") %>% 
  group_by(doi) %>% 
  mutate(aff = paste(whichinst, collapse = ";")) %>% 
  select(-whichinst) %>% 
  unique() %>%
  ungroup() %>% 
  select(-affiliation)

microbiome <- microbiome %>% 
  left_join(affcol, by = "doi")


# add POS attributes 

source("python/add-lemmas-save.R")


attrs <- microbiome %>% 
  group_by(doi) %>% 
  group_split() %>% 
  map(., safely(addattrs, otherwise = "fail"))

microbiome <- transpose(attrs) %>% 
  .$result %>% 
  .[. != "fail"] %>% 
  bind_rows()

# write to database

microbiomedb <- microbiome %>% 
  mutate(title = substr(title, 1, 450),
         container = substr(container, 1, 450),
         author = substr(author, 1, 1450),
         pubtype = substr(pubtype, 1, 45),
         researcharea = substr(researcharea, 1, 1450),
         funder = substr(funder, 1, 4500),
         affiliation = substr(affiliation, 1, 1450),
         aff = substr(aff, 1, 950),
         title_text = substr(title_text, 1, 450),
         title_lemma = substr(title_lemma, 1, 450),
         title_pos = substr(title_pos, 1, 240),
         title_dep = substr(title_dep, 1, 240),
         title_shape = substr(title_shape, 1, 240),
         abstract_text = substr(abstract_text, 1, 4500),
         abstract_lemma = substr(abstract_lemma, 1, 4500),
         abstract_pos = substr(abstract_pos, 1, 2400),
         abstract_dep = substr(abstract_dep, 1, 2400),
         abstract_shape = substr(abstract_shape, 1, 2400),
         body_text = substr(body_text, 1, 4500),
         body_lemma = substr(body_lemma, 1, 4500),
         body_pos = substr(body_pos, 1, 2000),
         body_dep = substr(body_dep, 1, 2000),
         body_shape = substr(body_shape, 1, 2000),
         affiliation = substr(affiliation, 1, 1250),
         author = substr(author, 1, 1250),
         researcharea = substr(researcharea, 1, 1250)) %>%  
  select(doi, title, container, published, author, pubtype, researcharea, funder, affiliation, aff, year, title_text, title_lemma,
         title_pos, title_dep, title_shape, abstract_text, abstract_lemma, abstract_pos, abstract_dep, abstract_shape,
         body_text, body_lemma, body_pos, body_shape) %>% 
  mutate(doi = tolower(doi))

sql <- config::get("sql1")

con <- DBI::dbConnect(odbc::odbc(),
                      driver = sql$driver,
                      server = sql$server,
                      database = sql$database,
                      port = sql$uid)

#DBI::dbSendQuery(con, "DELETE FROM microbiome")
DBI::dbAppendTable(con, "microbiome", microbiomedb)
