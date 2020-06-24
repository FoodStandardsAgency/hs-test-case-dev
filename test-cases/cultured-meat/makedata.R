
library(tidyverse)
devtools::load_all()

# load up WOS download

load("test-cases/cultured-meat/meat_clean.RData")

wos <- meat_clean %>% rename(abstract = abstract_text)

# fetch missing abstracts

noab <- wos %>% filter(abstract == "") %>% pull(doi) 

ablist <- map(noab, slowly(safely(getAbstract,otherwise = "fail"), rate = purrr::rate_delay(pause = 0.1)))

abstracts <- transpose(ablist) %>% 
  .$result %>% 
  .[. != "fail"] %>% 
  bind_rows() 

# bind in and filter for specific term "cultured meat"

wos <- wos %>% 
  left_join(abstracts, by = "doi") %>%
  replace(., is.na(.), "") %>% 
  mutate(abstract = if_else(abstract.y == "", abstract.x, abstract.y)) %>% 
  select(-abstract.x, -abstract.y) %>% 
  filter_at(., vars(title, abstract), any_vars(str_detect(., fixed("cultured meat", ignore_case = TRUE))))


# crossref search

dates <- seq.Date(as.Date("2009-02-01"), Sys.Date(), "months")
sinces <- dates[c(T,F)]
tos <- dates[c(F,T)]

# title search only - cannot search abstracts
# multiple words are treated as "OR" - cannot look for a phrase
# only searched for one phrase ("gut microbiome"), with title relevancy filter

crsearchresult <- map2(as.character(sinces), as.character(tos), safely(crSearch, otherwise = "fail"), query = "cultured meat")

crmeat <- transpose(crsearchresult) %>% 
  .$result %>% 
  .[. != "fail"] %>% 
  bind_rows() 

noab <- crmeat %>% 
  filter(is.na(abstract)) %>% 
  pull(doi)

ablist <- map(noab, slowly(safely(getAbstract,otherwise = "fail"), rate = purrr::rate_delay(pause = 0.1)))

abstracts <- transpose(ablist) %>% 
  .$result %>% 
  .[. != "fail"] %>% 
  bind_rows() 

crmeat <- crmeat %>% 
  left_join(abstracts, by = "doi") %>%
  replace(., is.na(.), "") %>% 
  mutate(abstract = if_else(abstract.y == "", abstract.x, abstract.y)) %>% 
  select(-abstract.x, -abstract.y) %>% 
  filter_at(., vars(title, abstract), any_vars(str_detect(., fixed("cultured meat", ignore_case = TRUE)))) %>% 
  mutate(body = "") %>% cleanac()


# bring together wos and crossref

wosdoi <- wos %>% mutate(doi = tolower(doi)) %>% pull(doi) %>% unique()

culturedmeat <- crmeat %>% 
  mutate(doi = tolower(doi)) %>% 
  filter(!doi %in% wosdoi) %>% 
  bind_rows(wos)


# cleaned up 'affiliation' column

cmaff <- culturedmeat %>% 
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

uimatch <- map_df(uilist$institution, function(x) cmaff %>% 
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

culturedmeat <- culturedmeat %>% 
  left_join(affcol, by = "doi")


# add POS attributes 

source("python/add-lemmas-save.R")


attrs <- culturedmeat %>% 
  group_by(doi) %>% 
  group_split() %>% 
  map(., safely(addattrs, otherwise = "fail"))

culturedmeat <- transpose(attrs) %>% 
  .$result %>% 
  .[. != "fail"] %>% 
  bind_rows()

# write to database

culturedmeat <- culturedmeat %>% 
  mutate(title = substr(title, 1, 450),
         container = substr(container, 1, 450),
         author = substr(author, 1, 1450),
         pubtype = substr(pubtype, 1, 45),
         researcharea = substr(researcharea, 1, 1450),
         funder = substr(funder, 1, 4500),
         affiliation = substr(affiliation, 1, 950),
         aff = substr(aff, 1, 1450),
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
         affiliation = substr(affiliation, 1, 1250),
         author = substr(author, 1, 1250),
         researcharea = substr(researcharea, 1, 1250)) %>%  
  select(doi, title, container, published, author, pubtype, researcharea, funder, affiliation, aff, year, title_text, title_lemma,
         title_pos, title_dep, title_shape, abstract_text, abstract_lemma, abstract_pos, abstract_dep, abstract_shape) %>% 
  mutate(doi = tolower(doi))

sql <- config::get("sql1")

con <- DBI::dbConnect(odbc::odbc(),
                      driver = sql$driver,
                      server = sql$server,
                      database = sql$database,
                      port = sql$uid)

#DBI::dbSendQuery(con, "DELETE FROM culturedmeat")
DBI::dbAppendTable(con, "culturedmeat", culturedmeat)

