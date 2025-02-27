#' Flowlines
#'
#' @description Process US flowline data. Before running this function, you must download NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z from https://www.epa.gov/waterdata/nhdplus-national-data. Unzip and call the path to the NHDPlusNationalData folder in the get_flowlines() function.
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param path (character) Path to location of data to extract
#' @param id.label (character) Column name of location ID
#'
#' @returns
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' dat <- get_flowlines(locs, path = 'data/', id.label = 'grid.id')
#' }

get_flowlines <- function(locs,
                          path,
                          id.label = "id") {

  tmp <- cleaning(locs, id.label, path)
  locs <- tmp$locs
  path <- tmp$path

  if ("NHDPlusNationalData" %in% list.files(path) == F) {
    stop("USA Flowlines file does not exist. \nStep 1: Download NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z from https://www.epa.gov/waterdata/nhdplus-national-data \nStep 2: Unzip, move NHDPlusNationalData folder to path, and try again")
  }


  if ("flowlines.rds" %in% list.files(paste0(path, "/NHDPlusNationalData/")) == F) {
    cat("Pre-processing of flowlines data has not been done yet. Pre-processing now.\n")

    # Load NHD flowline layer of National Database
    flowlines <- sf::st_read(paste0(path, "/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"),
                             layer='NHDFlowline_Network') %>%
      # Drop Z or M dimension
      st_zm() %>%
      # Transform CRS from 4269 to 3857
      st_transform(3857)

    # Save as RDS file
    saveRDS(flowlines, paste0(path, '/NHDPlusNationalData/flowlines.rds'))

  }

  cat("Loading flowlines file\n")
  flowlines <- readRDS(paste0(path, '/NHDPlusNationalData/flowlines.rds')) %>%

    # missing values coded as -9
    mutate(StreamOrde = ifelse(StreamOrde == -9, NA, StreamOrde))


  cat("Transforming\n")
  g1 <- locs %>%
    st_transform(st_crs(flowlines))

  cat("Intersecting\n")
  # Intersect national flowlines with grid
  st_intersection(g1, flowlines) %>%
    # Calculate stream length per grid and change to numeric
    dplyr::mutate(streamLength.m = st_length(.),
                  streamLength.m = as.numeric(streamLength.m)) -> tmp

  cat("Calculating total stream length and mean order\n")
  # Group at the grid-level and calculate stream length and average stream order
  tmp %>%
    filter(FTYPE == "StreamRiver") %>%
    group_by(id) %>%
    # By grid, calculate the total stream length (km) and
    # the average stream order, weighted by stream length
    summarize(streamLength.km = sum(streamLength.m, na.rm=T)/1000,
              streamOrd.mean = weighted.mean(StreamOrde, streamLength.m, na.rm=T)) %>%
    # Drop the flowline geometry
    st_drop_geometry() -> tmp1

  cat("Calculating stream length by stream order\n")
  # Group by grid and stream order to calculate stream length by stream order
  tmp %>%
    filter(FTYPE == "StreamRiver") %>%
    group_by(id, StreamOrde) %>%
    mutate(streamOrd = StreamOrde,
           streamOrd.sum = sum(streamLength.m)/1000) %>%
    ungroup() %>%
    st_drop_geometry() %>%
    select(id, streamOrd, streamOrd.sum) %>%
    distinct() %>%
    pivot_wider(id_cols = id,
                names_from = streamOrd, names_prefix = 'streamOrd',
                values_from = streamOrd.sum) %>% # in km
    #select(all_of(id.use), paste0('streamOrd',1:max(tmp$StreamOrde, na.rm=T)), streamOrdNA) %>%
    arrange(id) -> tmp2


  cat("Calculating artificial habitat (FTYPE = CanalDitch)\n")
  tmp %>%
    filter(FTYPE == "CanalDitch") %>%
    group_by(id) %>%
    # By grid, calculate the total stream length (km) and
    # the average stream order, weighted by stream length
    summarize(artificialLength.km = sum(streamLength.m, na.rm=T)/1000) %>%
    # Drop the flowline geometry
    st_drop_geometry() -> tmp3


  cat("Finishing up\n")
  full_join(tmp1, tmp2, by = "id") %>%
    full_join(tmp3, by = "id") -> grid.covs
  colnames(grid.covs)[1] <- id.label

  grid.covs <- locs %>%
    st_drop_geometry() %>%
    select(all_of(id.label)) %>%
    full_join(grid.covs, by = id.label)

  grid.covs[is.na(grid.covs)] <- 0

  # colnames(grid.covs)[1] <- "id"
  # grid.covs$id <- as.character(grid.covs$id)

  return(grid.covs)

}
