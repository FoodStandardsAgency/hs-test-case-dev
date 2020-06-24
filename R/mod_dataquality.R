# Module UI
  
#' @title   mod_dataquality_ui and mod_dataquality_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dataquality
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_dataquality_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns("dataquality"))
  )
}
    
# Module Server
    
#' @rdname mod_dataquality
#' @export
#' @import dplyr
#' @keywords internal
    
mod_dataquality_server <- function(input, output, session, data){
  ns <- session$ns

  getmis <- function(yourdata, col) {
    empty <- yourdata %>% 
      filter(is.na({{col}}) | {{col}} == "")
    if(nrow(empty) > 0) {
      empty %>% 
        mutate(mis = (nrow(empty) / nrow(yourdata)) * 100) %>% 
        pull(mis) %>% 
        unique() %>% 
        round(., 1)
    } else {
      return(0)
    }
  }
  
  getqual <- reactive({
    missing <- c(getmis(data(), container), 
                 getmis(data(), author), 
                 getmis(data(), funder), 
                 getmis(data(), affiliation),
                 getmis(data(), abstract_text),
                 getmis(data(), researcharea))
    mtib <- tibble(Variable = c("Journal", 
                                "Author",
                                "Funder",
                                "Author affiliation",
                                "Abstract",
                                "Research area"), 
                   `Missing (%)` = missing)
    return(mtib)
  })
  
  output$dataquality <- renderTable({
    getqual()
  })

}
    
## To be copied in the UI
# mod_dataquality_ui("dataquality_ui_1")
    
## To be copied in the server
# callModule(mod_dataquality_server, "dataquality_ui_1")
 
