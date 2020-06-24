# Microbiome test case

Files associated with the production of the microbiome test case.

`sources.csv` contains the sources that have created `academic.csv` - the article database

## Existing backlog

Articles retrieved from:

* Web of Science: all documents returned under the search terms “Gut microbiome”, "Gut flora", "Gut microbes", "Human gastrointestinal microbiota", "Gut metagenome", "Gut protozoa" or "faecal transplantation" published between 2010 and 2019 (returns 1006 articles with unique title and authors) (accessed 20.12.2019) (`wos_all.csv`)

## New searches and email ingestion

`get-searches.R` - script to retrieve relevant articles from PLoS

`get-email-articles.R` - script to retrieve relevant articles from email alerts

`scrapelog.csv` - when these scrapes have been carried out

`emails` - folder where alert emails are stored. Alerts ingested in this test 
case: 
* Google Scholar ("microbiome")
* PubMed alerts ("microbiome[Title/Abstract]")
* Individual journals - *Journal of Agricultural and Food Chemistry*, *Microbiome*, 
*Cell*, *Gut* and *Scientific Reports* (TOC alerts filtered for 
"Microbiome|Flora|Microbiota|metagenome|Human metagenome|gut flora|gut bacterium|gut microbes|gut protozoa|faecal transplantation" in 
title or abstract)
