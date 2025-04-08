#' Soil
#'
#' @description Download and process soil data. Source: geodata::soil_world()
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param var (character) Required argument for geodata::soil_world(). Valid options are: "bdod", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb".
#' @param depth (numeric) Required argument for geodata::soil_world(). Valid options are: 5, 15, 30, 60, 100, 200
#' @param stat (character) Required argument for geodata::soil_world(). Valid options are: "mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95". Ignored if var="wrb"
#' @param name (character) Required argument for geodata::soil_world(). One of "Acrisols", "Albeluvisols", "Alisols", "Andosols", "Arenosols", "Calcisols", "Cambisols", "Chernozems", "Cryosols", "Durisols", "Ferralsols", "Fluvisols", "Gleysols", "Gypsisols", "Histosols", "Kastanozems", "Leptosols", "Lixisols", "Luvisols", "Nitisols", "Phaeozems", "Planosols", "Plinthosols", "Podzols", "Regosols", "Solonchaks", "Solonetz", "Stagnosols", "Umbrisols", "Vertisols". Only used when var="wrb"
#' @param path (character) Path to location of data to extract
#' @param method (character) Method to extract data using terra::extract(). Valid methods are "fast" and "precise"
#'
#' @returns A data frame with summarized soil composition for each polygon in locs
#' @export
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_soil(locs, id.label = 'grid.id')
#' }

get_soil <- function(locs,
                     id.label,
                     var = c("bdod", "cfvo", "clay", "nitrogen", "ocd",
                             "phh2o", "sand", "silt", "soc"),
                     depth = 5,
                     stat = "mean",
                     name = "",
                     path = tempdir(),
                     method = "fast") {

  if (method %in% c("precise", "fast") == F) {
    stop('"method" must be "precise" or "fast"')
  }

  tmp <- cleaning(locs, id.label, path = "")
  locs <- tmp$locs


  # Get soil map
  soil.map <- geodata::soil_world(var = var,
                                  depth = depth,
                                  stat = stat,
                                  name = name,
                                  path = path)

  # Get things in order
  locs1 <- sf::st_transform(locs, sf::st_crs(soil.map))

  bb <- get_buffered_bbox(locs1)
  soil1 <- terra::crop(soil.map, bb)


  cat("Extracting data\n")
  if (method == "fast") {
    soil2 <- terra::zonal(soil1, terra::vect(locs1))
  } else if (method == "precise") {
    soil2 <- terra::zonal(soil1, terra::vect(locs1), weights = T)
  }

  # clean up
  soil3 <- dplyr::mutate(soil2, id = locs$id) %>% dplyr::select(id, dplyr::everything())

  colnames(soil3)[1] <- c(id.label)


  return(soil3)

}
