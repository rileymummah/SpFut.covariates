#' Buffered bounding box
#'
#' @description Get a buffered bounding box for set of locations
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#'
#' @returns A buffered bounding box (bbox object)
#' @export
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_buffered_bbox(locs)
#' }


get_buffered_bbox <- function(locs) {

  tmp <- sf::st_crs(locs, parameters = T)
  if (tmp$units_gdal == "degree") {
    buffer <- 1
  } else if (tmp$units_gdal == "metre") {
    buffer <- 2000
  } else {
    cat("Geometry units are neither 'degree' nor 'metre'. Please adjust and run again.")
  }

  bbo <- sf::st_bbox(locs)
  bbo[1] <- bbo[1] - buffer
  bbo[2] <- bbo[2] - buffer
  bbo[3] <- bbo[3] + buffer
  bbo[4] <- bbo[4] + buffer

  return(bbo)
}
