#' Travel time
#'
#' @description Download and process travel time data using \pkg{geodata}::\code{travel_time()}
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param to (character) Required argument for \pkg{geodata}::\code{travel_time()}. Valid options are "city" or "port"
#' @param size (numeric) Required argument for \pkg{geodata}::\code{travel_time()}. A positive integer indicating the size of the city or port. Can be between 1 and 9 if to="city" or between 1 and 5 if to="port".
#' @param up (logical) Required argument for \pkg{geodata}::\code{travel_time()}. If TRUE the travel time to a city of the size chosen or larger is returned.
#' @param path (character) Path to location of data to extract
#' @param method (character) Method to extract data using \pkg{terra}::\code{extract()}. Valid methods are "fast" and "precise"
#'
#' @returns A data frame with summarized travel time for each polygon in locs
#' @export
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_traveltime(locs, id.label = 'grid.id')
#' }


get_traveltime <- function(locs,
                           id.label,
                           to = "city",
                           size = 4,
                           up = T,
                           path = tempdir(),
                           method = "fast"){

  if (method %in% c("precise", "fast") == F) {
    stop('"method" must be "precise" or "fast"')
  }

  tmp <- cleaning(locs, id.label, path = "")
  locs <- tmp$locs

  # get traveltime map from geodata
  travel.map <- geodata::travel_time(to = to,
                                     size = size,
                                     up = up,
                                     path = path)

  # Get things in order
  locs1 <- sf::st_transform(locs, sf::st_crs(travel.map))

  bb <- get_buffered_bbox(locs1)
  travel1 <- terra::crop(travel.map, bb)


  cat("Extracting data\n")
  if (method == "fast") {
    travel2 <- terra::zonal(travel1, terra::vect(locs1))
  } else if (method == "precise") {
    travel2 <- terra::zonal(travel1, terra::vect(locs1), weights = T)
  }

  # clean up
  travel3 <- dplyr::mutate(travel2, id = locs$id) %>% dplyr::select(id, `travel_time_to_cities_1`)

  colnames(travel3) <- c(id.label, "traveltime")


  return(travel3)

}
