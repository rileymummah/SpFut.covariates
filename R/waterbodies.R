#' Waterbodies
#'
#' @description Process US waterbody data. Before running this function, you must download \emph{NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z} from \url{https://www.epa.gov/waterdata/nhdplus-national-data}. Unzip and call the path to the \emph{NHDPlusNationalData} folder in the \code{get_waterbodies()} function.
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param path (character) Path to location of data to extract
#' @param id.label (character) Column name of location ID
#'
#' @returns A data frame with summarized waterbodies for each polygon in locs
#' @export
#'
#' @importFrom sf st_read st_transform st_zm st_crs st_intersection st_drop_geometry
#' @importFrom tidyselect all_of
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_waterbodies(locs, path = 'data/', id.label = 'grid.id')
#' }

get_waterbodies <- function(locs,
                            path,
                            id.label = "id") {

  tmp <- cleaning(locs, id.label, path)
  locs <- tmp$locs
  path <- tmp$path


  if ("NHDPlusNationalData" %in% list.files(path) == F) {
    stop("USA Flowlines file does not exist. \nStep 1: Download NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z from https://www.epa.gov/waterdata/nhdplus-national-data \nStep 2: Unzip, move NHDPlusNationalData folder to path, and try again")
  }


  if ("flowlines.rds" %in% list.files(paste0(path, "/NHDPlusNationalData/")) == F) {
    cat("Pre-processing of waterbodies data has not been done yet. Pre-processing now.\n")

    # Load NHD flowline layer of National Database
    waterbody <- sf::st_read(paste0(path, "/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"),
                             layer='NHDWaterbody') %>%
      # Drop Z or M dimension
      sf::st_zm() %>%
      # Transform CRS from 4269 to 3857
      sf::st_transform(3857)

    # Save as RDS file
    saveRDS(waterbody, paste0(path, '/NHDPlusNationalData/waterbody.rds'))

  }

  cat("Loading waterbody file\n")
  lakepond <- readRDS(paste0(path, '/NHDPlusNationalData/waterbody.rds')) %>% dplyr::filter(FTYPE == "LakePond")

  # assuming -9998 means NA
  lakepond$MeanDepth[lakepond$MeanDepth == -9998] <- NA
  lakepond$LakeVolume[lakepond$LakeVolume == -9998] <- NA
  lakepond$MaxDepth[lakepond$MaxDepth == -9998] <- NA
  lakepond$MeanDUsed[lakepond$MeanDUsed == -9998] <- NA

  # categorize each lake
  lakepond <- dplyr::mutate(lakepond, size = dplyr::case_when(AREASQKM <= .01 ~ "verysmall",
                                                              AREASQKM > .01 &
                                                                AREASQKM <= 0.02 ~ "small",
                                                              AREASQKM > 0.02 &
                                                                AREASQKM <= 0.05 ~ "medium",
                                                              AREASQKM > 0.05 &
                                                                AREASQKM <= 10 ~ "large",
                                                              AREASQKM > 10 ~ "verylarge"))
  #table(lakepond$size)/nrow(lakepond)
  # These thresholds come from:
  #     this paper that I think defines ponds as <2ha (<0.02sqkm) Williams, P. J., J. Biggs, A. Crowe, J. Murphy, P. Nicolet, A. Weatherby, and M. Dunbar. 2010b. CS Technical Report No. 7/07 Countryside Survey: Ponds Report from 2007. Lancaster.
  #     somewhat even thresholds

  cat("Transforming\n")
  g1 <- sf::st_transform(locs, sf::st_crs(lakepond))

  cat("Intersecting\n")
  # Intersect national flowlines with grid
  tmp <- sf::st_intersection(g1, lakepond) %>%
          dplyr::mutate(area = sf::st_area(geometry)) %>%
          dplyr::group_by(id, size) %>%
          dplyr::summarize(area = as.numeric(sum(area)), .groups = "drop", n = dplyr::n()) %>%
          sf::st_drop_geometry() %>%
          tidyr::pivot_wider(names_from = size,
                             values_from = c(area, n),
                             values_fill = 0)


  grid.covs <- g1 %>%
                sf::st_drop_geometry() %>%
                dplyr::full_join(tmp, by = "id") %>%
                replace(is.na(.), 0) %>%
                dplyr::select(all_of(id.label), starts_with("area_"), starts_with("n_"))


  return(grid.covs)

}
