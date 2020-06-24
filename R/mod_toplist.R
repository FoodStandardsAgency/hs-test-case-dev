# Module UI

#' @title   mod_toplist_ui and mod_toplist_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_toplist
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_toplist_ui <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(tableOutput(ns("toplist")), type = 4, color = "#006F51", size = 0.3)
  )
}

# Module Server

#' @rdname mod_toplist
#' @export
#' @keywords internal
#' @import dplyr


mod_toplist_server <- function(input, output, session, data, var){
  ns <- session$ns
  
  t <- reactive({
      data() %>% 
      select({{var}}) %>%
      mutate({{var}} := stringr::str_split({{var}}, ";")) %>%
      tidyr::unnest(cols = {{var}}) %>%
      mutate({{var}} := trimws({{var}})) %>%
      filter({{var}} != "" & {{var}} != ",") 
    })

  
  output$toplist <- renderTable({
    validate(
      need(nrow(data()) != 0, "There are no journals with those criteria. Please change your filters."))
    sort(table(t(), dnn = c("","")), decreasing = TRUE)[1:3] 
  }, hover = TRUE, spacing = 'm', colnames = F)
}


## To be copied in the UI
# mod_toplist_ui("toplist_ui_1")

## To be copied in the server
# callModule(mod_toplist_server, "toplist_ui_1")
