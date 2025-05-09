% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elevation.R
\name{get_elevation}
\alias{get_elevation}
\title{Elevation}
\usage{
get_elevation(locs, path, id.label, method = "fast")
}
\arguments{
\item{locs}{(sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)}

\item{path}{(character) Path to location of data to extract}

\item{id.label}{(character) Column name of location ID}

\item{method}{(character) Method to extract data using \pkg{terra}::\code{extract()}. Valid methods are "fast" and "precise"}
}
\value{
A data frame with summarized elevation for each polygon in locs
}
\description{
Process USA elevation data using data from the Commission for Environmental Cooperation (CEC). Before running this function, you must download the USA elevation data from \url{http://www.cec.org/north-american-environmental-atlas/elevation-2023/}. Unzip and call the path to the \emph{elevation_tif} folder in the function \code{get_elevation()} function.
}
\examples{
\dontrun{
data(locs)

get_elevation(locs, path = 'data/', id.label = 'grid.id')
}
}
