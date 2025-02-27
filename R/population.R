#' Population density
#'
#' @description Download and process population density data. Source: geodata::population()
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param year (numeric) Year for geodata::population(). Valid years are 2000, 2005, 2010, 2015, and 2020
#' @param res (numeric) Resolution for geodata::population(). Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree)
#' @param path (character) Path to location of data to extract
#' @param method (character) Method to extract data using terra::extract. Valid methods are "fast" and "precise"
#'
#' @returns A data frame with summarized population density for each polygon in locs
#' @export
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' dat <- get_population(locs, id.label = 'grid.id')
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
  population.map <- geodata::population(year = year,
                                        res = res,
                                        path = path)

  # Get things in order
  locs1 <- sf::st_transform(locs, crs = sf::st_crs(population.map))

  bb <- get_buffered_bbox(locs = locs1)
  bb <- sf::st_bbox(locs1)
  pop1 <- terra::crop(population.map, bb)

  # terra::plot(pop1)
  # plot(locs1, add = T)


  cat("Extracting data\n")
  if (method == "fast") {
    pop2 <- terra::zonal(pop1, terra::vect(locs1))
  } else if (method == "precise") {
    pop2 <- terra::zonal(pop1, terra::vect(locs1), weights = T)
  }

  # clean up
  pop3 <- dplyr::mutate(pop2, id = locs$id)
  pop3 <- dplyr::select(pop3, id, population_density)

  colnames(pop3) <- c(id.label, "density")


  return(pop3)

}
