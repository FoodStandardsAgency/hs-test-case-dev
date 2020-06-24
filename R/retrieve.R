# RETRIEVE FUNCTIONS

# Fetch article information - metadata, abstract, body - from a DOI or title

#' Retrieve DOI from title
#'
#' Use crossref to retrieve information about an article via crossref from its title
#'
#' @param articletitle title of the article
#' @return A tibble with all the info that could be retrieved from crossref

fetchFromTitle <- function(articletitle) {
  rcrossref::cr_works(query = articletitle) %>%
    .$data %>%
    dplyr::filter(title == articletitle) %>%
    dplyr::slice(1)
}

#' Retrieve info from DOI
#'
#' Use crossref to retrieve information about an article via crossref from its DOI
#'
#' @param articledoi DOI of the article
#' @return A tibble with all the info that could be retrieved from crossref

fetchFromDOI <- function(articledoi) {
  rcrossref::cr_works(dois = articledoi) %>%
    .$data %>%
    dplyr::filter(doi == articledoi)
}

#' Get the body of an article
#'
#' Retrieve the full text of an article from a DOI, where access is available
#' Limited to first 30000 characters to write to CSV
#'
#' @param articledoi DOI identifier of the article
#' @return a tibble with DOI and body
#' @export

getBody <- function(articledoi) {
  tryCatch(fulltext::ft_get(articledoi) %>%
             fulltext::ft_collect() %>%
             pubchunks::pub_chunks("body") %>%
             pubchunks::pub_tabularize(bind = TRUE) %>%
             dplyr::as_tibble() %>%
             dplyr::mutate(doi = articledoi) %>%
             dplyr::select(doi, body) %>%
             dplyr::mutate(body = paste(body, collapse = " ")) %>%
             dplyr::mutate(body = substr(body, 1, 30000)) %>%
             unique(),
           error = function(e) dplyr::tibble(doi = articledoi, body = paste(e)),
           warning = function(e) dplyr::tibble(doi = articledoi, body = paste(e))) %>%
    dplyr::mutate(body = dplyr::if_else(grepl("no plugin", body), paste("no plugin", Sys.Date()), body)) %>%
    dplyr::mutate(body = dplyr::if_else(grepl("may not have access", body), paste("no access", Sys.Date()), body)) %>%
    dplyr::mutate(body = dplyr::if_else(grepl("must evaluate to|undefined columns selected", body),
                                        paste("no access", Sys.Date()), body))
}

#' Get an abstract
#'
#' Retrieve an abstract from a DOI (searches pubmed). Use slowly(getAbstract, rate = purrr::rate_delay(pause = 1)) if used in a map loop.
#'
#' @param articledoi DOI identifier of the article
#' @return a tibble with DOI and abstract
#' @export

getAbstract <- function(articledoi) {
  
    searchurl <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=",
                               articledoi,"[doi]&api_key=80d884725b2e1fa304a422d603912dac5c08")

  ID <- httr::GET(searchurl) %>%
    httr::content(.) %>%
    rvest::xml_nodes("Id") %>%
    rvest::html_text() %>%
    .[1]
  
  if(!is.na(ID)) {

    fetchurl <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
                       ID,"&rettype=abstract&api_key=80d884725b2e1fa304a422d603912dac5c08")

  abstract <- httr::GET(fetchurl) %>%
    httr::content() %>%
    rvest::xml_nodes("Abstract") %>%
    rvest::html_text()
  
  tibble::tibble(doi = articledoi, abstract = abstract)
  } else {
    tibble::tibble(doi = character(0), abstract = character(0))
    
  }
}
