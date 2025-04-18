% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning.R
\name{cleaning}
\alias{cleaning}
\title{Cleans locations, path, and polygon ID}
\usage{
cleaning(locs, id.label, path)
}
\arguments{
\item{locs}{(sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)}

\item{id.label}{(character) Column name of location ID}

\item{path}{(character) Path to location of data to extract}
}
\value{
A list of the locations and the path
}
\description{
Cleans locations, path, and polygon ID
}
\examples{
\dontrun{
data(locs)

cleaning(locs, id.label = 'grid.id', path = 'data/')
}
}
