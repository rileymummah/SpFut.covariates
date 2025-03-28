% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/protectedAreas.R
\name{get_protected}
\alias{get_protected}
\title{Protected areas}
\usage{
get_protected(locs, path, id.label = "id")
}
\arguments{
\item{locs}{(sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)}

\item{path}{(character) Path to location of data to extract}

\item{id.label}{(character) Column name of location ID}
}
\value{
A data frame with summarized protected areas for each polygon in locs
}
\description{
Process US protected area data. Before running this function, you must download PADUS4_0Geodatabase.zip from https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download. Unzip and call the path to thePADUS4_0Geodatabase folder in the get_protected() function.
}
\examples{
\dontrun{
data(locs)

dat <- get_protected(locs, path = 'data/', id.label = 'grid.id')
}
}
