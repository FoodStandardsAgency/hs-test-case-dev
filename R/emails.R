
#' Get link text from an email alert
#'
#' Function to return all link text in an email
#' @param emailfolder Path to folder containing email
#' @param email filename of email
#' @return A vector of text strings

getLinkText <- function(emailfolder,email) {
  xml2::read_html(paste0(emailfolder,"/",email)) %>%
    rvest::html_nodes("a") %>%
    rvest::html_text() %>%
    unique() %>%
    stringr::str_remove_all(., stringr::fixed("<i>", ignore_case = TRUE)) %>%
    stringr::str_remove_all(., stringr::fixed("</i>", ignore_case = TRUE)) %>%
    stringr::str_remove_all(., stringr::regex("\\.$")) %>%
    stringr::str_remove_all(., stringr::regex("[\r\n]"))
}

#' Extract list of DOIs from a vector of strings
#'
#' @param stringlist input vector of strings
#' @return a vector of strings that are all DOIs
#'
getDOIs <- function(stringlist) {
  if(sum(stringr::str_detect(stringlist, "^doi|^10\\.")) > 0) {
    stringlist %>%
      .[stringr::str_detect(., "^doi|^10\\.")] %>%
      stringr::str_remove(., stringr::fixed("doi.org/")) %>%
      tolower(.)
  } else {
    return(NULL)
  }
}

#' Extract list of titles (non-DOIs) from a vector of strings
#'
#' @param stringlist input vector of strings
#' @return a vector of strings that are not DOIs
#'
getTitle <- function(stringlist) {
  if(sum(stringr::str_detect(stringlist, "^doi|^10\\.")) > 0) {
    return(NULL)
  } else {
    stringlist %>%
      .[. != ""] %>%
      .[!stringr::str_detect(., "^doi")] %>%
      .[!stringr::str_detect(., "^10\\.")] %>%
      .[!stringr::str_detect(., stringr::fixed("https:"))] %>%
      .[!stringr::str_detect(., stringr::fixed("subscribe", ignore_case = TRUE))]
  }
}

#' Retrieve articles from a collection of emails
#' 
#' @param emailfolder Folder containing emails to scrape
#' @param existing Existing DOIs to avoid searching for if required
#' @return Clean tibble of all articles (likely to need to run getAbstract()
#'  and do a relevancy filter before adding anything to database)
#' @import purrr
#'  
getEmailArticles <- function(emailfolder, existing = NULL) {
  
  emails <- list.files(emailfolder)
  
  linktext <- map(emails, getLinkText, emailfolder = emailfolder) %>% unlist()
  dois <- map(linktext, getDOIs) %>% unlist() %>% unique() %>% .[!. %in% existing]
  articles1 <- map_df(dois, fetchFromDOI)
  titles <- map(linktext, getTitle) %>% unlist() %>% unique()
  articles2 <- map_df(titles, insistently(fetchFromTitle)) %>%
    dplyr::filter(!doi %in% existing & !doi %in% articles1$doi)
  
  dplyr::bind_rows(articles1, articles2) %>% 
    CrossRefCleanup()
}
