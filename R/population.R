#' Population density
#'
#' @description Download and process population density data using \pkg{geodata}::\code{population()}
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param year (numeric) Year for \pkg{geodata}::\code{population()}. Valid years are 2000, 2005, 2010, 2015, and 2020
#' @param res (numeric) Resolution for \pkg{geodata}::\code{population()}. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree)
#' @param path (character) Path to location of data to extract
#' @param method (character) Method to extract data using \pkg{terra}::\code{extract()}. Valid methods are "fast" and "precise"
#'
#' @returns A data frame with summarized population density for each polygon in locs
#' @export
#'
#' @importFrom rlang .data
#' @importFrom geodata population
#' @importFrom sf st_transform st_transform st_crs st_bbox
#' @importFrom terra zonal vect crop
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_population(locs, id.label = 'grid.id')
#' }


get_population <- function(locs,
                           id.label,
                           year = 2010,
                           res = 0.5,
                           path = tempdir(),
                           method = "fast") {

  if (method %in% c("precise", "fast") == F) {
    stop('"method" must be "precise" or "fast"')
  }

  tmp <- cleaning(locs, id.label, path = "")
  locs <- tmp$locs

  # get population map from geodata
  population.map <- population(year = year,
                               res = res,
                               path = path)

  # Get things in order
  locs1 <- st_transform(locs, crs = st_crs(population.map))

  bb <- get_buffered_bbox(locs = locs1)
  bb <- st_bbox(locs1)
  pop1 <- crop(population.map, bb)


  cat("Extracting data\n")
  if (method == "fast") {
    pop2 <- zonal(pop1, vect(locs1))
  } else if (method == "precise") {
    pop2 <- zonal(pop1, vect(locs1), weights = T)
  }

  # clean up
  pop3 <- mutate(pop2, id = locs$id)
  pop3 <- select(pop3, "id", "population_density")

  colnames(pop3) <- c(id.label, "density")


  return(pop3)

}
