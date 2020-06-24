# Module UI

#' @title   mod_filters_ui and mod_filters_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_filters
#'
#' @keywords internal
#' @export 
#' @import shinyWidgets
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
#'   
mod_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
    tabBox(
      title = NULL,
      id = "tabset1",
      width = 12,
      tabPanel("Date range", 
               sliderInput(ns("daterange"), 
                           "Select a date range", 
                           as.Date("2009-01-01"), as.Date("2020-01-01"), 
                           c(as.Date("2009-01-01"), as.Date("2020-01-01")))),
      tabPanel("Title and Abstract", 
               fluidRow(column(3,textInput(ns("mustinclude"), "Title or abstract must include this term")),
                        column(1, p("AND")),
                        column(3, textInput(ns("mustinclude2"), "Must also include")),
                        column(1, p("AND")),
                        column(3, textInput(ns("mustinclude3"), "Must also include"))
                        ),
               fluidRow(column(3,textInput(ns("mustexclude"), "Title or abstract with this term are excluded"))
                        )
               ),
      tabPanel("Authors",
               textInput(ns("authorinclude"), "Must include this author")
      ),
      tabPanel("Journals",
               textInput(ns("journalinclude"), "Must include this journal")
               )
      ),
    actionButton(ns("go"), "Get data/Apply filters"),
    withSpinner(textOutput(ns("nrow")), type = 4, color = "#006F51", size = 0.3)
    )
    )
}

# Module Server

#' @rdname mod_filters
#' @export
#' @keywords internal

mod_filters_server <- function(input, output, session, data){
  ns <- session$ns
  
  df <- reactive({data()})
  
  #include <- reactive({unlist(strsplit(input$mustinclude,","))})
   
  df <- eventReactive(input$go,{
    data() %>% 
      dplyr::filter(published > input$daterange[1] & published < input$daterange[2]) %>%
      dplyr::filter(grepl(gsub(" OR |OR", "|", input$mustinclude), title, ignore.case = TRUE) | grepl(gsub(" OR |OR", "|", input$mustinclude), abstract_text, ignore.case = TRUE)) %>%
      dplyr::filter(grepl(gsub(" OR |OR", "|", input$mustinclude2), title, ignore.case = TRUE) | grepl(gsub(" OR |OR", "|", input$mustinclude2), abstract_text, ignore.case = TRUE)) %>%
      dplyr::filter(grepl(gsub(" OR |OR", "|", input$mustinclude3), title, ignore.case = TRUE) | grepl(gsub(" OR |OR", "|", input$mustinclude3), abstract_text, ignore.case = TRUE)) %>%
      dplyr::filter(grepl(gsub(" OR |OR", "|", input$authorinclude), author, ignore.case = TRUE)) %>%
      dplyr::filter(grepl(gsub(" OR |OR", "|", input$journalinclude), container, ignore.case = TRUE)) %>%
      dplyr::filter(if (input$mustexclude != ""){
        !grepl(gsub(" OR |OR", "|",input$mustexclude), title, ignore.case = TRUE) & !grepl(gsub(" OR |OR", "|",input$mustexclude), abstract_text, ignore.case = TRUE)
      } else {
        grepl(input$mustexclude, title, ignore.case = TRUE) | grepl(input$mustexclude, abstract_text, ignore.case = TRUE) 
      }) 
    },
    ignoreNULL = T
    )
  
  output$nrow <- renderText({
    validate(
      need(nrow(df()) != 0, "There are no journals with those criteria. Please change your filters."))
    paste("Data loaded! With these filters there are", nrow(df()), "articles. You 
    can now visualise trends over time, most commonly occuring 
          words, and the key entities (authors, journals, etc.),  
          for the data you have selected")
  })
  
  return(df)
}

## To be copied in the UI
# mod_filters_ui("filters_ui_1")

## To be copied in the server
# callModule(mod_filters_server, "filters_ui_1")