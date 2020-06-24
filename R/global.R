#' Make a lemma table
#'
#' @param data The dataset
#' @param col A string with the column you want the lemma table for
#' @return A count table of lemmas by pos
#' @import dplyr
maketable <- function(data, col) {
  data %>%
    select(contains({{col}})) %>%
    select(contains(c("lemma", "pos"))) %>%
    rename(lemma = 1, pos = 2) %>%
    mutate_all(., ~stringr::str_split(., ",")) %>%
    tidyr::unnest(cols = names(.)) %>%
    count(lemma, pos)
}


#' Make a wordcloud
#'
#' @param wordtable A table with words and counts (n)
#' @param wordvar The variable with the words
#' @param size Upper limit of word size
#' @import dplyr
#'
makewordcloud <- function(wordtable, wordvar, size = 4) {
  wordlist <- wordtable %>% pull({{wordvar}})
  wordcloud::wordcloud(words = wordlist,
                       freq = wordtable$n,
                       scale = c(size,0.5),
                       min.freq = 20,
                       max.words=200, random.order=FALSE,
                       random.color = FALSE, rot.per = 0,
                       colors=RColorBrewer::brewer.pal(7, "Greens"))
}
