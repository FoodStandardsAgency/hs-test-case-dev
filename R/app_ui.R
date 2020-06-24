#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(skin = "green",
                  dashboardHeader(title = "Publication analysis"),
                  dashboardSidebar(
                    sidebarMenu(menuItem("Welcome", tabName = "welcome"),
                                menuItem("Get data", tabName = "getdata"),
                                menuItem("Trends over time", tabName = "timetrends"),
                                menuItem("Word frequency", tabName = "wordclouds"),
                                menuItem("Key entities", tabName = "entities"),
                                menuItem("Data quality", tabName = "dataquality")
                    )
                  ),
                  dashboardBody(
                    tags$head(includeHTML(("google-analytics.html"))),
                    tabItems(
                      tabItem(tabName = "welcome",     
                              h3("Welcome"),
                              wellPanel(
                                p("Welcome to the FSA horizon scanning tool. This tool will support 
                                the Strategic Projects Team horizon scanning process which aims to 
                                provide insight of trends in food related issues that may impact the 
                                FSA in the 5 to 10 year timeframe. This tool allows the easy visualisation 
                                of trends in academic research, like number of publications. Key terms, 
                                topics, authors, funders and research institutions can be easily identified, 
                                and results can be filtered by search term and publication date for more 
                                specific insight."),
                                p("The tool is still under development, and three 'test cases' have been 
                                  created to demonstrate its capabilities. They are illustrative only and 
                                  should not be taken to represent a systematic appraisal of the literature 
                                  on these topics."),
                                p("Once you have selected a test case, you can get the data by going to the 'Get Data' 
                                  tab, where you can also add some further filters."),
                                p("Questions? Feedback? Want to use this tool? Please get in touch at datascience@food.gov.uk"),
                                mod_select_ui("select_ui_1")
                              )
                              ),
                      tabItem(tabName = "getdata",
                              h3("Get data"),
                              wellPanel(
                                p("All fields are optional. Filters are not case sensitive. 
                                Multiple search terms can be separated with with OR."),
                                p("Please press the Get data/Apply filters button after you have specified 
                                  your filter terms, to load the data."),
                                br(),
                                mod_filters_ui("filters_ui_1")
                              )
                              ),
                      
                      tabItem(tabName = "timetrends",
                              h3("Trends over time"), 
                              wellPanel(
                                p("Trends in the number of scientific journal papers published every month.")
                                ),
                              wellPanel(
                                mod_timetrends_ui("timetrends_ui_1")
                                )
                              ),
                      
                      tabItem(tabName = "wordclouds", 
                              h3("Word frequency"),
                              mod_wordfreq_ui("wordfreq_ui_1")),
                      
                      tabItem(tabName = "entities", 
                              h3("Top 3"),
                                  wellPanel(
                                    fluidRow(
                                      tabBox(
                                        title = NULL,
                                        id = "tabset1",
                                        width = 12,
                                        tabPanel("Author", mod_toplist_ui("toplist_ui_1")),
                                        tabPanel("Affiliation", mod_toplist_ui("toplist_ui_5")),
                                        tabPanel("Journal", mod_toplist_ui("toplist_ui_2")),
                                        tabPanel("Research area", mod_toplist_ui("toplist_ui_3")),
                                        tabPanel("Funder", mod_toplist_ui("toplist_ui_4"))
                                  )
                                  )
                                ),
                             h3("Detailed tables"),
                             wellPanel(
                               strong("View detailed information for a selected entity"),
                               mod_tables_ui("tables_ui_1") 
                             )
                      ),
                      tabItem(tabName = "dataquality",
                              h3("Data quality"),
                              wellPanel(
                              p("How complete is the data for your selection?"),
                              br(),
                              mod_dataquality_ui("dataquality_ui_1")
                              )
                      )
            
                              
                    )
                  )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'hstestcase')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
