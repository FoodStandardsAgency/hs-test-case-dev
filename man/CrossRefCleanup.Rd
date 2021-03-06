% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning-functions.R
\name{CrossRefCleanup}
\alias{CrossRefCleanup}
\title{Clean up table returned from CrossRef API}
\usage{
CrossRefCleanup(datatable)
}
\arguments{
\item{datatable}{A table returned from FetchFromTitle() or FetchFromDOI()}
}
\value{
A tibble with the 13 fields that go in the database
}
\description{
Clean up table returned from CrossRef API
}
