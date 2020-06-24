# Module UI

#' @title   mod_tables_ui and mod_tables_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_tables
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("tblselect"), label = NULL, 
                   c("Author", "Author affiliation", "Journal", "Research area", "Funder")),
    withSpinner(DT::dataTableOutput(ns("entitytable")), type = 4, color = "#006F51", size = 0.3)
  )
}

# Module Server

#' @rdname mod_tables
#' @export
#' @keywords internal
#' @import dplyr

mod_tables_server <- function(input, output, session, data){
  ns <- session$ns
  
  lookup <- tibble(tblselect = c("Author", "Author affiliation", "Journal", "Research area", "Funder"),
                   variables = c("author", "aff", "container", "researcharea", "funder"))
  
  # count the entity
  
  getcount <- reactive({
    var <- lookup %>% filter(tblselect == input$tblselect) %>% .[1,2] %>% as.character()
    ec <- data() %>% 
      select(var)%>%
      mutate(newc := stringr::str_split(.[[1]], ";"))%>%
      tidyr::unnest(cols = newc) %>%
      mutate(newc := trimws(newc)) %>%
      filter(newc != "" & newc != ",")%>% 
      count(newc) %>%
      arrange(desc(n))
    names(ec) <- c(input$tblselect, "n")
    return(ec)
  })
  
  # produce a table
  
  output$entitytable <- DT::renderDataTable({
    validate(
      need(nrow(data()) != 0, "There are no journals with those criteria. Please change your filters."))
    DT::datatable(getcount())
  })
  
}

## To be copied in the UI
# mod_tables_ui("tables_ui_1")

## To be copied in the server
# callModule(mod_tables_server, "tables_ui_1")

