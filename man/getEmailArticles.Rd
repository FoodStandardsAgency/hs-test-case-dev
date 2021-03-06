% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emails.R
\name{getEmailArticles}
\alias{getEmailArticles}
\title{Retrieve articles from a collection of emails}
\usage{
getEmailArticles(emailfolder, existing = NULL)
}
\arguments{
\item{emailfolder}{Folder containing emails to scrape}

\item{existing}{Existing DOIs to avoid searching for if required}
}
\value{
Clean tibble of all articles (likely to need to run getAbstract()
 and do a relevancy filter before adding anything to database)
}
\description{
Retrieve articles from a collection of emails
}
