# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "filters" ) # Module to allow selection of filtered dataset
golem::add_module( name = "timetrends") # Module to show the time trend graphs
golem::add_module( name = "wordfreq") # Module to make word cloud
golem::add_module( name = "tables" ) # Module to make top entities tables
golem::add_module( name = "toplist" ) # Module to show top entities only
golem::add_module( name = "importance") # Module to show importance
golem::add_module( name = "select") # Module to select test case
golem::add_module( name = "dataquality") # Module to produce data quality metrics
## 2.2 Add dependencies

#usethis::use_package( "thinkr" ) # To call each time you need a new package

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

#golem::browser_button()

## 2.5 Add external files

#golem::add_js_file( "script" )
#golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
#usethis::use_vignette("hstestcase")
#devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
#usethis::use_github()
#usethis::use_travis()
#usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
