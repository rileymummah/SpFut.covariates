#' Landcover
#'
#' @description Process US landcover data. Before running this function, you must download the USA landcover data from http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/. Call the path to the usa_land_cover_2020_30m_tif folder in the get_landcover() function.
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param path (character) Path to location of data to extract
#' @param id.label (character) Column name of location ID
#'
#' @returns A data frame with summarized landcover for each polygon in locs
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' dat <- get_landcover(locs, path = 'data/', id.label = 'grid.id')
#' }

get_landcover <- function(locs,
                          path,
                          id.label = "id") {


  tmp <- cleaning(locs, id.label, path)
  locs <- tmp$locs
  path <- tmp$path

  if ("usa_land_cover_2020v2_30m_tif" %in% list.files(path) == F) {
    stop("USA Landcover file does not exist. Download from http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/, move usa_land_cover_2020v2_30m_tif folder to path, and try again")
  }

  # load data
  # these are stored locally but can be downloaded from http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/
  usa <- terra::rast(paste0(path, "/usa_land_cover_2020v2_30m_tif/USA_NALCMS_landcover_2020v2_30m/data/USA_NALCMS_landcover_2020v2_30m.tif"))

  # give locs same crs as raster
  locs1 <- locs %>% sf::st_transform(sf::st_crs(usa))

  # Extract landcover values
  cat("Extracting landcover values\n")
  bb1 <- sf::st_bbox(locs1)
  sub <- terra::crop(usa, bb1)
  sub1 <- terra::mask(sub, locs1)

  # terra::plot(sub1)
  # terra::plot(locs1, add = T)

  all <- terra::extract(sub1, locs1, fun = table)

  # clean up
  cat("Cleaning\n")


  # Fill in landcover types that are missing
  all1 <- all

  lctypes <- as.character(1:19)
  missingtypes <- lctypes[-which(lctypes %in% colnames(all1))]

  for (m in 1:length(missingtypes)) {
    all1[,(ncol(all1)+1)] <- NA
    colnames(all1)[ncol(all1)] <- missingtypes[m]
  }


  # Organize
  all1[,id.label] <- locs1$conus.grid.id

  all1 <- all1 %>% dplyr::select(dplyr::all_of(id.label), dplyr::all_of(as.character(1:19)))
  all1[is.na(all1)] <- 0

  total <- rowSums(all1[,2:20])
  all1[,2:20] <- all1[,2:20]/total

  colnames(all1) <- c(id.label,
                      "temperate/subpolar needleleaf forest",
                      "subpolar taiga needleleaf forest",
                      "tropical/subtropical broadleaf evergreen forest",
                      "tropical/subtropical broadleaf deciduous forest",
                      "temperature/subpolar broadleave deciduous forest",
                      "mixed forest",
                      "tropical/subtropical shrubland",
                      "temperate/subpolar shrubland",
                      "tropical/subtropical grassland",
                      "temperature/subpolar grassland",
                      "subpolar/polar shrubland-lichen-moss",
                      "subpolar/polar grassland-lichen-moss",
                      "subpolar/polar barren-lichen-moss",
                      "wetland",
                      "cropland",
                      "barren",
                      "urban/built",
                      "water",
                      "snow/ice")

  all1$forest <- rowSums(all1[,2:7])
  all1$shrub <- rowSums(all1[,c(8, 9, 12)])
  all1$grass <- rowSums(all1[,c(10, 11, 13)])
  all1$barren <- rowSums(all1[,c(14, 17)])

  landcover <- all1
  row.names(landcover) <- 1:nrow(landcover)

  return(landcover)

}

