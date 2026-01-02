#' Human footprint
#'
#' @description Download and process human footprint data using \pkg{geodata}::\code{footprint()}
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param year (numeric) Year for \pkg{geodata}::\code{footprint()}. Valid years are 1993 or 2009.
#' @param path (character) Path to location of data to extract
#' @param method (character) Method to extract data using \pkg{terra}::\code{extract()}. Valid methods are "fast" and "precise"
#'
#' @returns A data frame with summarized human footprint for each polygon in locs
#' @export
#'
#' @importFrom rlang .data
#' @importFrom geodata footprint
#' @importFrom sf st_transform
#' @importFrom terra crop zonal vect
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_footprint(locs, id.label = 'grid.id')
#' }


get_footprint <- function(locs,
                          id.label,
                          year = 2009,
                          path = tempdir(),
                          method = "fast") {

  if (method %in% c("precise", "fast") == F) {
    stop('"method" must be "precise" or "fast"')
  }

  tmp <- cleaning(locs, id.label, path = "")
  locs <- tmp$locs

  # get footprint map from geodata
  footprint.map <- footprint(year = year,
                                      path = path)

  # Get things in order
  locs1 <- st_transform(locs, st_crs(footprint.map))

  bb <- get_buffered_bbox(locs1)
  foot1 <- crop(footprint.map, bb)


  cat("Extracting data\n")
  if (method == "fast") {
    foot2 <- zonal(foot1, vect(locs1))
  } else if (method == "precise") {
    foot2 <- zonal(foot1, vect(locs1), weights = T)
  }

  # clean up
  foot3 <- mutate(foot2, id = locs$id) %>%
            select("id", "wildareas-v3-2009-human-footprint")

  colnames(foot3) <- c(id.label, "footprint")


  return(foot3)
}
