# Module UI

#' @title   mod_wordfreq_ui and mod_wordfreq_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_wordfreq
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_wordfreq_ui <- function(id){
  
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(5,
               selectInput(ns("column"), 
                    "Select what you want the produce the wordcloud for",
                    choices = c("title", "abstract"),
                    selected = "title"),
               selectInput(ns("exword"), 
                    "Words to exclude:", 
                    choices = hstestcase::exwords, 
                    selected = hstestcase::exwords, 
                    multiple = TRUE),
              textInput(ns("addex"), "Add more words:"),
              actionButton(ns("addnow"), "Add word")),
       column (7,
               withSpinner(plotOutput(ns("wordfreq")), type = 4, color = "#006F51", size = 0.3),
               sliderInput(ns("wcsize"), "Alter the size of the words", min = 2, max = 7, value = 4, step = 0.25))
       )
      )
    )
}

# Module Server

#' @rdname mod_wordfreq
#' @export
#' @keywords internal
#' @import purrr
#' @import dplyr
#' @import wordcloud

mod_wordfreq_server <- function(input, output, session, data){
  ns <- session$ns
  
  observeEvent(input$addnow, {
    newchoices <- append(input$exword, input$addex)
    updateSelectInput( session, "exword", choices = newchoices, selected = newchoices )
  })
  
  lemmatable <- reactive({
    lt <- maketable(data(), input$column) %>% 
      filter(!lemma %in% input$exword )
    return(lt)
  })
  
  # make word cloud
  
  lemmacloud <- reactive({ 
    lc <- makewordcloud(lemmatable(), lemma, input$wcsize)
    return(lc)
  })
  
  output$wordfreq <- renderPlot({
    validate(
      need(nrow(data()) != 0, "There are no journals with those criteria. Please change your filters."))
    lemmacloud()
  })
  
  return(list(lt = lemmatable))
}

## To be copied in the UI
# mod_wordfreq_ui("wordfreq_ui_1")

## To be copied in the server
# callModule(mod_wordfreq_server, "wordfreq_ui_1")

