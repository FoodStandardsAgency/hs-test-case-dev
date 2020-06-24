
# Clean, NLP, save to database

reticulate::source_python("python/python-functions.py")

#' Get POS table for a given column
#' 
#' @param data The data table with columns to be parsed
#' @param col The desired column to parse
#' @return A dataset with doi and POS variables prefixed with column name (e.g. title_lemma)
#' @import dplyr
#' @import purrr

getparse <- function(data, col) {
  colname <- deparse(substitute(col))
  strings <- data %>% filter(!is.na({{col}})) %>% pull({{col}})
  stringids <- data %>% filter(!is.na({{col}})) %>% pull(doi)
  postable <- strings %>%
    map(., get_token_attributes) %>%
    purrr::map_depth(., 2, ~purrr::set_names(., c("text","lemma","pos","tag","dep","shape","alpha","stop"))) %>%
    map(., bind_rows)
  if(nrow(postable[[1]]) > 0) {
    postable %>%
      map(., ttt) %>%
      bind_rows() %>%
      rename_all(., ~paste0(colname,"_",.)) %>%
      bind_cols(doi = stringids, .)
  } else {
    return(tibble(doi = ""))
  }
}

#' Tidy POS table
#' 
#' Convert all to lower case
#' Remove stop words, non-alphanumeric, small words
#' Change everything tagged as proper noun to noun (default but can be set to false)
#' 
#' @param table A table representing one title/abstract/body, 
#' and each table row representing one token within it
#' @param nounconvert logical indicating whether to reclassify proper nouns as nouns
#' @return A single row tibble, with a comma-separated list of POS in each column (text, lemmas, etc.
#' )
#' @import dplyr

ttt <- function(table, nounconvert = TRUE) {
  table %>%
    mutate(text = tolower(text), lemma = tolower(lemma)) %>%
    filter(stop == FALSE) %>%
    filter(alpha == TRUE) %>%
    filter(!shape %in% c("x", "X", "xx", "XX", "Xx", "xX")) %>%
    mutate(pos = if_else(nounconvert == TRUE & pos == "PROPN", "NOUN", pos)) %>%
    unique() %>%
    mutate_all(., ~paste(., collapse = ",")) %>%
    unique() %>%
    select(text, lemma, pos, dep, shape)
}

#' Add POS colums to clean data and save to database
#' 
#' @param table Table of article metadata to which you want to add POS tags
#' @param destination Where you want to save the file to

addattrs <- function(table) {
  ac <- table %>% 
    #extra cleaning step to remove odd characters that will make python crash
    dplyr::mutate_at(vars(title, abstract, body), ~stringr::str_remove_all(., "[^[:alnum:][:blank:]?&/\\-]"))
  
  titleparse <- getparse(ac, title)
  abparse <- getparse(ac, abstract)
  bodyparse <- getparse(ac, body)
  
  ac %>%
    dplyr::left_join(., titleparse, by = "doi") %>%
    dplyr::left_join(., abparse, by = "doi") %>%
    dplyr::left_join(., bodyparse, by = "doi") 
}







