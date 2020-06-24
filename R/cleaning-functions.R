# CLEAN FUNCTIONS

# Clean data fetched from searches and add parts of speech prior to saving to database


#' Clean up table returned from CrossRef API
#'
#' @param datatable A table returned from FetchFromTitle() or FetchFromDOI()
#' @return A tibble with the 13 fields that go in the database

CrossRefCleanup <- function(datatable) {
  
  if("author" %in% names(datatable)) {
    authorinfo <- datatable %>%
      dplyr::select(doi, author) %>%
      tidyr::unnest(cols = author, keep_empty = TRUE) %>%
      dplyr::mutate_all(., tidyr::replace_na, "") %>%
      dplyr::mutate(author = {if("given" %in% names(.) & "family" %in% names(.)) paste0(family,",",given) else ""}) %>%
      dplyr::mutate(atemp = "") %>% # make a temp variable so the next line doesn't fail!
      tidyr::unite("aff", {if("affiliation.name" %in% names(.)) dplyr::contains("affiliation") else "atemp"}, sep = " ; ") %>%
      dplyr::mutate(aff = stringr::str_remove_all(aff, "^[; ]+")) %>%
      dplyr::mutate(aff = stringr::str_remove_all(aff, "[; ]+$")) %>%
      dplyr::mutate(aff = stringr::str_remove_all(aff, "[\r\n]")) %>%
      dplyr::group_by(doi) %>%
      dplyr::mutate(author = paste(author, collapse = " ; ")) %>%
      dplyr::mutate(aff = paste0(aff, collapse = " ; ")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(aff = stringr::str_replace(aff, "^[ ;]+$", "")) %>%
      dplyr::mutate(aff = stringr::str_replace_all(aff, "(;){2,}", ";")) %>%
      dplyr::select(doi, author, affiliation = aff) %>%
      unique()
  } else {
    authorinfo <- tibble::tibble(doi = datatable$doi, author = "", affiliation = "")
  }
  
  if("funder" %in% names(datatable)) {
    funder <- datatable %>%
      dplyr::select(doi, funder) %>%
      tidyr::unnest(cols = funder, keep_empty = TRUE) %>%
      dplyr::mutate_all(., tidyr::replace_na, "") %>%
      dplyr::mutate(funder = {if("name" %in% names(.)) paste0(name) else ""}) %>%
      dplyr::group_by(doi) %>%
      dplyr::mutate(funder = paste(funder, collapse = " ; ")) %>%
      dplyr::ungroup() %>%
      dplyr::select(doi, funder) %>%
      unique()
  } else {
    funder <- tibble::tibble(doi = datatable$doi, funder = "")
  }
  
  if("assertion" %in% names(datatable)) {
    compint <- datatable %>%
      dplyr::select(doi, assertion) %>%
      dplyr::mutate(assertion = purrr::map_if(assertion, is.null, ~ tibble::tibble())) %>%
      tidyr::unnest(cols = assertion, keep_empty = TRUE) %>%
      dplyr::mutate(group.label = {if("group.label" %in% names(.)) paste0(group.label) else ""}) %>%
      dplyr::filter(group.label == "Competing interests") %>%
      dplyr::mutate(value = {if("value" %in% names(.)) paste0(value) else ""}) %>%
      dplyr::select(doi, compint = value)
  } else {
    compint <- tibble::tibble(doi = datatable$doi, compint = "")
  }
  datatable %>%
    dplyr::mutate(doi = {if("doi" %in% names(.)) paste0(doi) else ""}) %>%
    dplyr::mutate(title = {if("title" %in% names(.)) paste0(title) else ""}) %>%
    dplyr::mutate(container = {if("container.title" %in% names(.)) paste0(container.title) else ""}) %>%
    dplyr::mutate(pubtype = {if("type" %in% names(.)) paste0(type) else ""}) %>%
    dplyr::mutate(issue = {if("issue" %in% names(.)) paste0(issue) else ""}) %>%
    dplyr::mutate(volume = {if("volume" %in% names(.)) paste0(volume) else ""}) %>%
    dplyr::mutate(pubdate = {if("issued" %in% names(.)) paste0(issued) else ""}) %>%
    dplyr::mutate(abstract = {if("abstract" %in% names(.)) paste0(abstract) else ""}) %>%
    dplyr::mutate(researcharea = {if("subject" %in% names(.)) paste0(subject) else ""}) %>%
    dplyr::mutate(researcharea = stringr::str_replace_all(researcharea, ",", ";")) %>% 
    dplyr::select(doi, title, pubdate, container, volume, issue, pubtype, abstract, researcharea) %>%
    dplyr::left_join(authorinfo, by = "doi") %>%
    dplyr::left_join(funder, by = "doi") %>%
    dplyr::left_join(compint, by = "doi") %>%
    dplyr::mutate_all(., dplyr::na_if, "NA") %>%
    dplyr::mutate_all(., tidyr::replace_na, "")
}

#' Genreal cleanup 
#'
#' @param datatable Data table to be cleaned

cleanac <- function(datatable) {
  library(magrittr)
  
  # check each line is unique DOI - function will fail if not
  
  assertthat::assert_that(datatable %>% dplyr::filter(grepl("^10", doi)) %>% nrow() == nrow(datatable),
                          msg = "there is not a DOI for each object")
  assertthat::assert_that(datatable %>% dplyr::select(doi) %>% unique() %>% nrow() == nrow(datatable),
                          msg = "non-unique DOIs")
  
  datatable %>%
    
    # if date is YYYY-MM-DD floor to start of month
    # if date is YYYY-MM add -01
    # dates not either YYYY-MM-DD or YYYY-MM will be missing

    dplyr::mutate(ymd = dplyr::if_else(grepl("([0-9]){4}\\-([0-9]){2}\\-([0-9]){2}", pubdate), pubdate, "")) %>%
    dplyr::mutate(ymd = dplyr::if_else(grepl("^([0-9]){4}\\-([0-9]){2}$", pubdate), paste0(pubdate,"-01"), ymd)) %>%
    dplyr::mutate(published = lubridate::floor_date(as.Date(ymd), "month")) %>%
    dplyr::select(-pubdate) %>% 
    
    # create a year variable as an alternative timeline
    
    dplyr::mutate(year = as.numeric(substr(published, 1, 4))) %>%
    dplyr::select(-ymd) %>%
    
    # make sure all articles are 2010 or later
    
    dplyr::filter(year >= 2010) %>%
    
    #affiliation tidying (remove authors, rogue-semi colons, and duplicates within DOI)
    
    dplyr::mutate(affiliation = stringr::str_remove_all(affiliation, "\\[[A-z ,;'\\.\\-]+\\]")) %>% 
    dplyr::mutate(affiliation = stringr::str_remove_all(affiliation, "^[; ]+")) %>%
    dplyr::mutate(affiliation = stringr::str_remove_all(affiliation, "[; ]+$")) %>% 
    dplyr::group_by(doi) %>% 
    dplyr::mutate(affiliation = stringr::str_split(affiliation, ";")) %>% 
    tidyr::unnest(cols = "affiliation") %>% 
    dplyr::mutate(affiliation = trimws(affiliation)) %>% 
    unique() %>% 
    dplyr::mutate(affiliation = stringr::str_c(affiliation, collapse = ";")) %>% 
    unique() %>% 
    dplyr::ungroup() %>% 
    
    # basic cleanup for containers to avoid duplication due to different capitalisations etc.
    # (downside of this is that things that should be capitalised are now not -
    # ideally want something more rigorous based on ISSN?)
    
    dplyr::mutate(container = stringr::str_to_title(container)) %>%
    
    # remove jats tags, newline and "abstract" from abstracts
    
    dplyr::mutate(abstract = tolower(abstract)) %>%
    dplyr::mutate(abstract = stringr::str_remove_all(abstract, "<[A-z:]+>")) %>%
    dplyr::mutate(abstract = stringr::str_remove_all(abstract, "</[A-z:]+>")) %>%
    dplyr::mutate(abstract = stringr::str_remove(abstract, stringr::fixed("abstract"))) %>%
    dplyr::mutate(abstract = stringr::str_remove_all(abstract, "\\n")) %>%
    dplyr::mutate(abstract = trimws(abstract)) %>%
    
    # replace as NA any body that is no plugin/access
    
    dplyr::mutate(body = ifelse(grepl("^no access", body), NA, body)) %>%
    dplyr::mutate(body = ifelse(grepl("^no plugin", body), NA, body))
}
