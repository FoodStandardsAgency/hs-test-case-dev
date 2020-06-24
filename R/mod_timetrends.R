# Module UI

#' @title   mod_timetrends_ui and mod_timetrends_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_timetrends
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_timetrends_ui <- function(id){
  ns <- NS(id)
  tagList(
    #actionButton(ns("go2"), "Plot"),
    withSpinner(plotOutput(ns("timetrend")), type = 4, color = "#006F51", size = 0.3),
    #actionButton(ns("addTSbutton"), "Add a trendline"),
    textInput(ns("addTS"), "Show the trendline for a specific term in your selected data")
  )
}

# Module Server

#' @rdname mod_timetrends
#' @export
#' @keywords internal
#' @import ggplot2

mod_timetrends_server <- function(input, output, session, data, data2){
  ns <- session$ns
  
  timeplot <- function(dataset) {
    p <- dataset %>% 
      #dplyr::group_by(published) %>%
      #dplyr::summarise(`number of articles` = n()) %>%
      #dplyr::filter(!is.na(published)) %>%
      ggplot(aes(x = as.Date(published), y = `number of articles`, color = group)) + 
      geom_line(size=1) +
      scale_color_manual(values=c("grey", "black", "red"))+
      theme_minimal() +
      xlab("Date published")
    return(p)
  }
  
  d1 <- reactive({ 
      data() %>%
      dplyr::group_by(published) %>%
      dplyr::summarise(`number of articles` = n()) %>%
      dplyr::filter(!is.na(published)) %>%
      dplyr::mutate(group = "Filtered articles")
   })
  d2 <- reactive({
      data2() %>% 
      dplyr::group_by(published) %>%
      dplyr::summarise(`number of articles` = n()) %>%
      dplyr::filter(!is.na(published)) %>%
      dplyr::filter(published < as.Date("2020-01-01")) %>% 
      dplyr::mutate(group = "All articles")
    })
  #d3<- reactive({
  #  rbind(d1(),d2())
  #})
  
  dt <- reactive({ #eventReactive(input$addTSbutton,{ 
      data() %>% 
        dplyr::filter(grepl(input$addTS, title, ignore.case = TRUE) | grepl(input$addTS, abstract_text, ignore.case = TRUE))%>%
        dplyr::group_by(published) %>%
        dplyr::summarise(`number of articles` = n()) %>%
        dplyr::filter(!is.na(published))%>%
        dplyr::mutate(group = "Selected term") 
      })
  
   d3<- reactive({
       if (input$addTS != ""){
         rbind(d1(),d2(),dt())
         } else {
         rbind(d1(),d2())
           }
       })
  
  output$timetrend <- renderPlot({
    validate(
      need(nrow(data()) != 0, "There are no journals with those criteria. Please change your filters."))
    timeplot(d3())
  })
  
  
  
}

## To be copied in the UI
# mod_timetrends_ui("timetrends_ui_1")

## To be copied in the server
# callModule(mod_timetrends_server, "timetrends_ui_1")

