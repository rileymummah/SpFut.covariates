#' Climate data
#'
#' @description
#' Download and process climate data using \pkg{geodata}::\code{worldclim_global()}
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param res (numeric) Required argument for \pkg{geodata}::\code{worldclim_global()}. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a degree).
#' @param path (character) Path to location of data to extract
#' @param method (character) Method to extract data using \pkg{terra}::\code{extract()}. Valid methods are "fast" and "precise"
#'
#' @returns A data frame with summarized minimum temperature, maximum temperature, and total precipitation for each polygon in locs
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom geodata worldclim_global
#' @importFrom terra crop zonal vect
#' @importFrom sf st_transform st_drop_geometry st_crs st_bbox
#' @importFrom dplyr mutate select full_join
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
  tmin.map <- worldclim_global(res = res,
                               var = "tmin",
                               path = path)

  tmax.map <- worldclim_global(res = res,
                               var = "tmax",
                               path = path)

  prec.map <- worldclim_global(res = res,
                               var = "prec",
                               path = path)

  # stack em up and crop to study region
  maps <- c(tmin.map, tmax.map, prec.map)

  bb <- st_transform(locs, st_crs(maps)) %>% st_bbox()
  maps1 <- crop(maps, bb)

  # transform grid to crs of maps
  g1 <- st_transform(locs, st_crs(maps))

  locs <- st_drop_geometry(locs)


  cat("Extracting data\n")
  if (method == "fast") {
    clim <- zonal(maps1, vect(g1))
  } else if (method == "precise") {
    clim <- zonal(maps1, vect(g1), weights = T)
  }


  # separate variables
  tmin1 <- clim[,c(1:12)]
  tmax1 <- clim[,c(13:24)]
  prec1 <- clim[,c(25:36)]

  tmin1$tmin <- apply(tmin1, 1, min)
  tmin2 <- mutate(tmin1, id = g1$id) %>% select("id", "tmin")


  tmax1$tmax <- apply(tmax1, 1, max)
  tmax2 <- mutate(tmax1, id = g1$id) %>% select("id", "tmax")


  prec1$prec <- apply(prec1, 1, sum)
  prec1$spr.prec <- apply(prec1[,3:5], 1, sum)
  prec2 <- mutate(prec1, id = g1$id) %>% select("id", "prec", "spr.prec")

  clim <- full_join(tmin2, tmax2, by = "id") %>%
            full_join(prec2, by = "id")

  colnames(clim)[1] <- id.label

  return(clim)


}
