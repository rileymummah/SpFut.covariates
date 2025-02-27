#' Cleans locations, path, and polygon ID
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param path (character) Path to location of data to extract
#'
#' @returns A list of the locations and the path
#' @export
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' cleaning(locs, id.label = 'grid.id', path = 'data/')
#' }

cleaning <- function(locs, id.label, path) {

  id.ind <- grep(id.label, colnames(locs))
  locs$id <- dplyr::pull(locs, id.label)

  if (substr(path, nchar(path), nchar(path)) == "/") path <- substr(path, 1, nchar(path)-1)

  dat <- list(locs = locs,
              path = path)
}







# could this be faster by extracting all locations at once?


# get_riparian <- function(locs,
#                          path,
#                          stream.buffer = 100,
#                          chunksize = 100,
#                          break.every = 10) {
#
#   check_for_id(locs)
#
#
#   # load data once
#   if ("usa_land_cover_2020_30m_tif" %in% list.files(path) == F) {
#     stop("USA Landcover file does not exist. Download from http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/, move usa_land_cover_2020_30m_tif folder to path, and try again")
#   }
#
#
#   cat("Loading data\n")
#   # load NLCD
#   # these are stored locally but can be downloaded from http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/
#   usa <- terra::rast(paste0(path, "usa_land_cover_2020_30m_tif/USA_NALCMS_landcover_2020_30m/data/USA_NALCMS_landcover_2020_30m.tif"))
#
#   # give locs same crs as raster
#   locs <- locs %>%
#     st_transform(st_crs(usa))
#
#
#   # Now load streams
#   if ("NHDPlusNationalData" %in% list.files(path) == F) {
#     stop("USA Flowlines file does not exist. Download from https://www.epa.gov/waterdata/nhdplus-national-data, move NHDPlusNationalData folder to path, and try again")
#   }
#
#
#   if ("flowlines.rds" %in% list.files(paste0(path, "NHDPlusNationalData/")) == F) {
#     print("Pre-processing of flowlines data has not been done yet. Get comfortable, this takes a while. If this gives an error, I apologize")
#
#
#
#     # Load NHD flowline layer of National Database
#     flowlines <- sf::st_read(paste0(path, "NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"),
#                              layer='NHDFlowline_Network') %>%
#       # Drop Z or M dimension
#       st_zm() %>%
#       # Transform CRS from 4269 to 3857
#       st_transform(3857)
#
#     # Save as RDS file
#     saveRDS(flowlines, paste0(path, 'NHDPlusNationalData/flowlines.rds'))
#
#
#   }
#
#   flowlines <- readRDS(paste0(path, 'NHDPlusNationalData/flowlines.rds')) %>%
#     # missing values coded as -9
#     mutate(StreamOrde = ifelse(StreamOrde == -9, NA, StreamOrde)) %>%
#     st_transform(st_crs(usa))
#
#
#   # Now make a grid to separate locs into chunks
#   chunkgrid <- st_make_grid(locs, cellsize = st_area(locs[1,])*chunksize) %>%
#     st_sf() %>%
#     mutate(chunk.id = 1:nrow(.))
#
#   cat(paste0("There are ", nrow(chunkgrid), " chunks\n"))
#
#   locs <- st_join(locs, chunkgrid) %>%
#
#     # each cell overlaps with more than one chunkcell, but only need calculate it once
#     group_by(id) %>%
#     slice_head(n = 1)
#
#
#   # Loop through each chunk
#   all <- c()
#   for (k in 1:nrow(chunkgrid)) {
#
#     # get cells in each chunk
#     locs1 <- locs %>%
#       filter(chunk.id == k)
#     cat(paste0("Chunk ", k, " contains ", nrow(locs1), " cells\n"))
#
#     if (nrow(locs1) == 0) next
#
#     #cat("Cropping spatial data\n")
#     bb <- st_bbox(locs1)
#
#     # crop raster to bbox of entire region
#     usa1 <- terra::crop(usa, bb)
#     usa1[usa1 == 0] <- NA
#
#     # crop streams
#     subriv <- st_crop(flowlines, bb)
#
#     # add a buffer to streams
#     #cat("Adding buffer to streams\n")
#     flowbuffer <- subriv %>%
#       st_buffer(dist = stream.buffer)
#
#
#     # loop through each cell
#
#     for (l in 1:nrow(locs1)) {
#
#       #loc <- locs1[l,]
#
#       tmp <- terra::mask(usa1, flowbuffer)
#       tmp <- terra::mask(tmp, locs1[l,])
#       #terra::plot(locs1[l,1])
#       #terra::plot(tmp, add = T)
#
#       tmp1 <- terra::as.data.frame(tmp, xy = T)
#
#
#       if (nrow(tmp1) == 0) { # if there are no streams, there is no habitat
#         lc <- data.frame(id = locs1$id[l])
#       } else {
#         lc <- data.frame(id = locs1$id[l],
#                          table(tmp1$USA_NALCMS_landcover_2020_30m)) %>%
#           pivot_wider(names_from = Var1, values_from = Freq)
#       }
#
#       all <- bind_rows(all, lc)
#
#     }
#
#   } # end chunk loop
#
#
#
#   # clean up
#   cat("Cleaning\n")
#
#   all1 <- all
#
#   lctypes <- as.character(1:19)
#   missingtypes <- lctypes[-which(lctypes %in% colnames(all1))]
#
#   if (length(missingtypes) > 0) {
#     for (m in 1:length(missingtypes)) {
#       all1[,(ncol(all1)+1)] <- NA
#       colnames(all1)[ncol(all1)] <- missingtypes[m]
#     }
#   }
#
#
#   all1 <- all1 %>%
#     select(id, all_of(as.character(1:19)))
#   all1[is.na(all1)] <- 0
#
#   # total <- rowSums(all1[,2:20])
#   # all1[,2:20] <- all1[,2:20]/total
#
#
#   colnames(all1) <- c("id",
#                       paste0("stream_",
#                              c("temperate/subpolar needleleaf forest",
#                                "subpolar taiga needleleaf forest",
#                                "tropical/subtropical broadleaf evergreen forest",
#                                "tropical/subtropical broadleaf deciduous forest",
#                                "temperature/subpolar broadleave deciduous forest",
#                                "mixed forest",
#                                "tropical/subtropical shrubland",
#                                "temperate/subpolar shrubland",
#                                "tropical/subtropical grassland",
#                                "temperature/subpolar grassland",
#                                "subpolar/polar shrubland-lichen-moss",
#                                "subpolar/polar grassland-lichen-moss",
#                                "subpolar/polar barren-lichen-moss",
#                                "wetland",
#                                "cropland",
#                                "barren",
#                                "urban/built",
#                                "water",
#                                "snow/ice")))
#
#   all1$stream_forest <- rowSums(all1[,2:7])
#   all1$stream_shrub <- rowSums(all1[,c(8, 9, 12)])
#   all1$stream_grass <- rowSums(all1[,c(10, 11, 13)])
#   all1$stream_barren <- rowSums(all1[,c(14, 17)])
#
#   all1$id <- as.character(all1$id)
#
#   landcover <- all1
#
#   return(landcover)
#
#   #return(all)
#
# }





# get_iNat_supp <- function(locs,
#                           commonName,
#                           space) {
#
#   # load inat data
#   inatsupp <- read.csv(paste0("data/species/", commonName, "/iNat-supp.csv"))
#
#   inat <- st_as_sf(inatsupp,
#                    coords = c("lon", "lat"),
#                    crs = 4326)
#
#   # if space = discrete
#   if (space == "discrete")  {
#     # load full grid because we need the geometry
#     load("data/USA/grid-and-huc.rdata")
#
#     locs <- filter(usa.grid, usa.grid.id %in% locs$usa.grid.id)
#
#     # project
#     l1 <- st_transform(locs, crs = st_crs(inat))
#
#     # intersect
#     inat1 <- inat %>%
#       st_join(l1, join = st_within) %>%
#       st_drop_geometry() %>%
#       as.data.frame()
#
#     # summarize
#     inat2 <- inat1 %>%
#       group_by(usa.grid.id) %>%
#       filter(is.na(usa.grid.id) == F) %>%
#       summarize(inat.rec = n())
#
#     # clean
#     inat2 <- inat2 %>%
#       mutate(usa.grid.id = as.character(usa.grid.id))
#
#     return(inat2)
#   }
#
#
#
#   # if space = continuous
#   if (space == "continuous") {
#     # project
#     l1 <- st_transform(locs, crs = st_crs(inat))
#
#     # intersect
#     inat1 <- inat %>%
#       st_join(l1, join = st_within) %>%
#       st_drop_geometry() %>%
#       as.data.frame()
#
#     # summarize
#     inat2 <- inat1 %>%
#       group_by(id) %>%
#       filter(is.na(id) == F) %>%
#       summarize(inat.rec = n())
#
#     # clean
#     inat2 <- inat2 %>%
#       mutate(id = as.character(id))
#
#     return(inat2)
#
#   }
#
# }

