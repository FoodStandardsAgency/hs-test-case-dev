#' @import shiny
app_server <- function(input, output,session) {
  
  # Get the filtered data
  acdata <- callModule(mod_select_server, "select_ui_1")
  df <- callModule(mod_filters_server, "filters_ui_1", data = acdata)
  
  # Do stuff with the filtered data
  
  # plot of trends over time
  
  callModule(mod_timetrends_server, "timetrends_ui_1", data = df, data2 = acdata)
  
  # wordcloud
  
  lems <- callModule(mod_wordfreq_server, "wordfreq_ui_1", data = df)
  
  # tables of key entities
  callModule(mod_toplist_server, "toplist_ui_1", data = df, var = author)
  callModule(mod_toplist_server, "toplist_ui_2", data = df, var = container)
  callModule(mod_toplist_server, "toplist_ui_3", data = df, var = researcharea)
  callModule(mod_toplist_server, "toplist_ui_4", data = df, var = funder)
  callModule(mod_toplist_server, "toplist_ui_5", data = df, var = aff)
  
  callModule(mod_tables_server, "tables_ui_1", data = df)
  
  # data quality metrics
  
  callModule(mod_dataquality_server, "dataquality_ui_1", data = df)
  

}
