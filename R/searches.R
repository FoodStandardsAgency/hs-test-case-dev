# SEARCH FUNCTIONS

# Query databases for search terms

#' Searches for a term in PLOS since a given publication date
#'
#' @param query text of the query
#' @param since date, default is today minus a week
#' @param limit maximum number of articles to return (default = 100)
#' @export
#' @examples
#' # search for just the word "microbiome" in all fields
#' plosSearch("microbiome")
#' # search for the term "gut microbiome"
#' plosSearch("gut microbiome")
#' # search for the words "gut" and "microbiome" within 10 words of each other
#' plosSearch("'gut microbiome'~10")
#'

plosSearch <- function(query, since = Sys.Date() - 7, limit = 100) {
  search <- fulltext::ft_search(query,
                      from = c("plos"),
                      plosopts = list(fl = c("publication_date",
                                             "abstract",
                                             "title",
                                             "author",
                                             "id",
                                             "article_type",
                                             "affiliate",
                                             "competing_interest",
                                             "financial_disclosure",
                                             "journal",
                                             "volume",
                                             "issue",
                                             "subject",
                                             "body"),
                                      sort='publication_date desc'),
                      limit = limit)
  results <- search$plos$data

  if(nrow(results) > 0) {
    search$plos$data %>%
      dplyr::filter(publication_date > as.Date(since)) %>%
      dplyr::mutate(pubdate = as.Date(publication_date)) %>%
      dplyr::select(-publication_date) %>%
      dplyr::rename(doi = id,
                    container = journal,
                    pubtype = article_type,
                    affiliation = affiliate,
                    compint = competing_interest,
                    researcharea = subject,
                    funder = financial_disclosure) %>%
      dplyr::mutate_all(., ~stringr::str_remove_all(., "\n")) %>%
      dplyr::mutate_all(., ~trimws(.)) %>% 
      dplyr::mutate(body = substr(body, 1, 30000)) %>% 
      
      # tidy up research area
      
      dplyr::mutate(researcharea = stringr::str_replace_all(researcharea, "/", ",")) %>% 
      dplyr::mutate(researcharea = stringr::str_replace_all(researcharea, "[,]+", ";")) %>% 
      dplyr::mutate(researcharea = stringr::str_split(researcharea, ";")) %>% 
      tidyr::unnest(cols = "researcharea") %>% 
      dplyr::mutate(researcharea = stringr::str_to_sentence(trimws(researcharea))) %>% 
      dplyr::group_by(doi) %>% 
      unique() %>% 
      dplyr::mutate(researcharea = paste(researcharea, collapse = ";")) %>% 
      dplyr::ungroup() %>% 
      unique() %>% 
      dplyr::mutate(researcharea = stringr::str_remove(researcharea, "^;"))
      
  } else {
    return(results)
  }
}



#' Function to perform crossref search on a query term for a given time window
#' 
#' Some records will return an abstract but coverage will be vastly improved
#' by running getAbstract() where they are missing
#' 
#' @param query Query term
#' @param since Date articles published since (default is today minus a week)
#' @param until Maximum date (default is today plus 1 year)
#' @param relevant string of terms (separated by |) to insist on in title
#' @return A tibble of article metadata, unless over 1000 articles are returned in 
#' which case it asks the user to modify their search
#' 
crSearch <- function(query, 
                     since = as.character(Sys.Date() - 7), 
                     until = as.character(Sys.Date() + 365),
                     relevant = "") {
  
  search <- fulltext::ft_search(query,
                                from = c("crossref"),
                                crossrefopts = list(filter = c(from_pub_date=since, until_pub_date = until)),
                                limit = 1000)
  found <- search$crossref$found
  if(found == 0) {
    tibble(doi = character())
  } else if(found > 1000) {
    print("over 1000 records, please select a smaller date window")
  } else {
    results <- search$crossref$data %>%
      CrossRefCleanup() %>%     
      filter(grepl(relevant, title, ignore.case = T))
    return(results)
  }
}




#' Searches for a term in pubmed since a given publication date
#' 
#' 

