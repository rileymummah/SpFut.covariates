% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/landcover.R
\name{get_landcover}
\alias{get_landcover}
\title{Landcover}
\usage{
get_landcover(locs, path, id.label = "id")
}
\arguments{
\item{locs}{(sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)}

\item{path}{(character) Path to location of data to extract}

\item{id.label}{(character) Column name of location ID}
}
\value{
A data frame with summarized landcover for each polygon in locs
}
\description{
Process US landcover data. Before running this function, you must download the USA landcover data from http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/. Call the path to the usa_land_cover_2020_30m_tif folder in the get_landcover() function.
}
\examples{
\dontrun{
data(locs)

dat <- get_landcover(locs, path = 'data/', id.label = 'grid.id')
}
}
