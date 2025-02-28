#' Climate data
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param res (numeric) Required argument for geodata::worldclim_global(). Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree).
#' @param path (character) Path to location of data to extract
#' @param method (character) Method to extract data using terra::extract. Valid methods are "fast" and "precise"
#'
#' @returns A data frame with summarized minimum temperature, maximum temperature, and total precipitation for each polygon in locs
#' @export
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_climate(locs, id.label = 'grid.id')
#' }


get_climate <- function(locs,
                        id.label,
                        res = 0.5,
                        path = tempdir(),
                        method = "fast") {

  if (method %in% c("precise", "fast") == F) {
    stop('"method" must be "precise" or "fast"')
  }

  tmp <- cleaning(locs, id.label, path = "")
  locs <- tmp$locs



  # download rasters
  tmin.map <- geodata::worldclim_global(res = res,
                                        var = "tmin",
                                        path = path)

  tmax.map <- geodata::worldclim_global(res = res,
                                        var = "tmax",
                                        path = path)

  prec.map <- geodata::worldclim_global(res = res,
                                        var = "prec",
                                        path = path)

  # stack em up and crop to study region
  maps <- c(tmin.map, tmax.map, prec.map)

  bb <- sf::st_transform(locs, sf::st_crs(maps)) %>% sf::st_bbox()
  maps1 <- terra::crop(maps, bb)

  # transform grid to crs of maps
  g1 <- sf::st_transform(locs, sf::st_crs(maps))

  locs <- sf::st_drop_geometry(locs)


  cat("Extracting data\n")
  if (method == "fast") {
    clim <- terra::zonal(maps1, terra::vect(g1))
  } else if (method == "precise") {
    clim <- terra::zonal(maps1, terra::vect(g1), weights = T)
  }


  # separate variables
  tmin1 <- clim[,c(1:12)]
  tmax1 <- clim[,c(13:24)]
  prec1 <- clim[,c(25:36)]

  tmin1$tmin <- apply(tmin1, 1, min)
  tmin2 <- dplyr::mutate(tmin, id = g1$id) %>% dplyr::select(id, tmin)


  tmax1$tmax <- apply(tmax1, 1, max)
  tmax2 <- dplyr::mutate(tmax, id = g1$id) %>% dplyr::select(id, tmax)


  prec1$prec <- apply(prec1, 1, sum)
  prec1$spr.prec <- apply(prec1[,3:5], 1, sum)
  prec2 <- dplyr::mutate(prec1, id = g1$id) %>% dplyr::select(id, prec, spr.prec)

  clim <- dplyr::full_join(tmin2, tmax2, by = "id") %>%
            dplyr::full_join(prec2, by = "id")

  colnames(clim)[1] <- id.label

  return(clim)


}
