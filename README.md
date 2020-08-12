
# hstestcase

<!-- badges: start -->
<!-- badges: end -->

Dashboard to display interactive summary statistics for literature.

## Accessing the dashboard

The dashboard is deployed at https://foodstandards.shinyapps.io/hstestcase/ 

## Developing the dashboard

* The dashboard was built in R 3.6.2 and RStudio 1.2, using the {golem} framework
* Package dependencies (R and Python) are captured using {renv}, which stores 
the required R packages in a project-specific library, and the python 
requirements in *environment.yaml*. To restore the library, run `renv::restore()`

## What's here

### Create data

Functions in `R/`:
* functions to search PLOS and Crossref in *searches.R*
* functions to retrieve article information from a folder of emails (saved as HTML) in *emails.R*
* functions to retrieve article metadata from a DOI or title in *retrieve.R*
* functions in *cleaning-functions.R* make the data from different sources clean and consistent 
prior to uploading to the database

The specific usage of these is in `test-cases/` - each test case has its own 
folder with a *makedata.R* file showing how each test case was assembled through 
a mixture of search functions and uploading search results and emails.

The *makedata.R* files call upon python functions. The `python/` folder contains 
a python file to define these (*python-functions.py*), and an R file
(*add-lemmas-save.R*) which is sourced at the end of each makedata file.

The *makedata.R* files write to a SQL database (credentials in config.yml file not on github)

The app fetches the data from the SQL server.
The only data that lives in the `data/` folder is a list of terms that are excluded 
from word counts because they are generic terms such as 'conclusion' or 'abstract'.

### The Shiny app and its modules

to run the app, you will need the config.yaml file with the database credentials - 
this is not pushed to git! If you are running the app locally you will also 
need to be connected to Forticlient.

the interface and server functions are separated into *app_ui.R* and *app_server.R*

most functionality is contained within modules:
* *mod_select.R* fetches the references for the chosen test case
* *mod_filters.R* applies the user's chosen filters to the data - this filtered 
data is what is passed to the other modules
* *mod_timetrends.R* plots the freqency of articles (total + filtered + for a 
chosen sub-trend) over time
* *mod_wordfreq.R* produces a wordcloud with the most commonly occurring terms 
in the title/abstract
* *mod_toplist.R* displays the top authors/journals/funders/research areas/author 
affiliations and *mod_tables.R* is a paginated table with all of these in order

### Other things

The `progress/` folder contains presentations from the show and tell sessions 
held during the development of the app.
