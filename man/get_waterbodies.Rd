% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waterbodies.R
\name{get_waterbodies}
\alias{get_waterbodies}
\title{Waterbodies}
\usage{
get_waterbodies(locs, path, id.label = "id")
}
\arguments{
\item{locs}{(sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)}

\item{path}{(character) Path to location of data to extract}

\item{id.label}{(character) Column name of location ID}
}
\value{
A data frame with summarized waterbodies for each polygon in locs
}
\description{
Process US waterbody data. Before running this function, you must download \emph{NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z} from \url{https://www.epa.gov/waterdata/nhdplus-national-data}. Unzip and call the path to the \emph{NHDPlusNationalData} folder in the \code{get_waterbodies()} function.
}
\examples{
\dontrun{
data(locs)

get_waterbodies(locs, path = 'data/', id.label = 'grid.id')
}
}
