# Module UI
  
#' @title   mod_select_ui and mod_select_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_select
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    p(),
    selectInput(ns("case"),label="Choose a test case",
                choices= c("Microbiome","Cultured meat", "Food contact materials")),
    p(withSpinner(textOutput(ns("description")), type = 4, color = "#006F51", size = 0.3))
    #add description for secelcted case
  )
}
    
# Module Server
    
#' @rdname mod_select
#' @export
#' @keywords internal
    
mod_select_server <- function(input, output, session){
  ns <- session$ns
  
  output$description <- renderText({
    if (input$case == "Microbiome"){
      paste("A personalised diet to influence the gut microbiome 
            can be perceived as another form of personalised medicine. 
            Researchers have found that gut bacterial communities 
            may be linked to the difficulty some people have losing weight, 
            and they could play a role in cardiovascular disease. 
            The microbiome also seems to be intimately tied to the 
            immune system, and thus it plays an important role in 
            immune-related diseases and disorders, including allergies.")
    } else if (input$case == "Cultured meat") {
      paste("Cultured/ Lab-grown 
            meat aims to produce ethical and cheaper meat with a lower 
            carbon footprint than conventional meat. Meat is grown in the 
            laboratory from animal cells, reducing the need to slaughter animals.")
    } else if (input$case == "Food contact materials") {
      paste("Food contact materials 
            include materials like food packing and cooking utensils. 
            There is ongoing development of new materials, e.g. using 
            algae-derived material for biodegradable ‘plastic’ bottles.")
    } else {
      paste("Please select a topic.")
    }
  })

  
  sql <- config::get("sqlcred")
  
  pool <- pool::dbPool(odbc::odbc(),
                 Driver = sql$driver,
                 Server = sql$server,
                 Port = sql$port,
                 UID = sql$uid,
                 PWD = sql$pwd,
                 MultipleActiveResultSets = "True",
                 Database = sql$database, 
                 encoding = "latin1")

acdata <- reactive({
  if (input$case == "Microbiome"){
    acdata <- DBI::dbGetQuery(pool, "SELECT * FROM microbiome") %>% dplyr::as_tibble()
    return(acdata)
  } else if (input$case == "Cultured meat") {
    acdata <- DBI::dbGetQuery(pool, "SELECT * FROM culturedmeat") %>% dplyr::as_tibble()
    return(acdata)
  } else if (input$case == "Food contact materials") {
    acdata <- DBI::dbGetQuery(pool, "SELECT * FROM fcm") %>% dplyr::as_tibble()
    return(acdata)
  } else {
    
  }
  
})    
  
}
## To be copied in the UI
# mod_select_ui("select_ui_1")
    
## To be copied in the server
# callModule(mod_select_server, "select_ui_1")
 
