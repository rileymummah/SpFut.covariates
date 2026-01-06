#' Waterbodies
#'
#' @description Process US waterbody data. Before running this function, you must download \emph{NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z} from \url{https://www.epa.gov/waterdata/nhdplus-national-data}. Unzip and call the path to the \emph{NHDPlusNationalData} folder in the \code{get_waterbodies()} function.
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param path (character) Path to location of data to extract
#' @param id.label (character) Column name of location ID
#' @param size (list) A list of labeled thresholds to categorize waterbody size
#'
#' @returns A data frame with summarized waterbodies for each polygon in locs
#' @export
#'
#' @importFrom rlang .data
#' @importFrom utils globalVariables
#' @importFrom magrittr "%>%"
#' @importFrom sf st_read st_transform st_zm st_crs st_intersection st_drop_geometry st_area
#' @importFrom tidyselect all_of
#' @importFrom dplyr mutate n filter case_when
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_waterbodies(locs, path = 'data/', id.label = 'grid.id')
#' }


get_waterbodies <- function(locs,
                            path,
                            size = list(verysmall = c(0, 0.01),
                                        small = c(0.01, 0.02),
                                        medium = c(0.02, 0.05),
                                        large = c(0.05, 10),
                                        verylarge = c(10, Inf)),
                            id.label = "id") {

  tmp <- cleaning(locs, id.label, path)
  locs <- tmp$locs
  path <- tmp$path


  if ("NHDPlusNationalData" %in% list.files(path) == F) {
    stop("USA Flowlines file does not exist. \nStep 1: Download NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z from https://www.epa.gov/waterdata/nhdplus-national-data \nStep 2: Unzip, move NHDPlusNationalData folder to path, and try again")
  }


  if ("waterbody.rds" %in% list.files(paste0(path, "/NHDPlusNationalData/")) == F) {
    cat("Pre-processing of waterbodies data has not been done yet. Pre-processing now.\n")

    # Load NHD flowline layer of National Database
    waterbody <- st_read(paste0(path, "/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"),
                             layer='NHDWaterbody') %>%
      # Drop Z or M dimension
      st_zm() %>%
      # Transform CRS from 4269 to 3857
      st_transform(3857)

    # Save as RDS file
    saveRDS(waterbody, paste0(path, '/NHDPlusNationalData/waterbody.rds'))

  }

  cat("Loading waterbody file\n")
  lakepond <- readRDS(paste0(path,'/NHDPlusNationalData/waterbody.rds')) %>%
                filter(.data$FTYPE == "LakePond")

  # assuming -9998 means NA
  lakepond$MeanDepth[lakepond$MeanDepth == -9998] <- NA
  lakepond$LakeVolume[lakepond$LakeVolume == -9998] <- NA
  lakepond$MaxDepth[lakepond$MaxDepth == -9998] <- NA
  lakepond$MeanDUsed[lakepond$MeanDUsed == -9998] <- NA

  # categorize each lake
  # lakepond <- mutate(lakepond, size = case_when(AREASQKM <= .01 ~ "verysmall",
  #                                               AREASQKM > .01 &
  #                                                 AREASQKM <= 0.02 ~ "small",
  #                                               AREASQKM > 0.02 &
  #                                                 AREASQKM <= 0.05 ~ "medium",
  #                                               AREASQKM > 0.05 &
  #                                                 AREASQKM <= 10 ~ "large",
  #                                               AREASQKM > 10 ~ "verylarge"))
  lakepond$size <- NA
  for (s in 1:length(size)) {
    lakepond$size[which(lakepond$AREASQKM >= size[[s]][1] & lakepond$AREASQKM < size[[s]][2])] <- names(size)[s]
  }
  lakepond <- filter(lakepond, is.na(.data$size) == F)

  #table(lakepond$size)/nrow(lakepond)
  # These thresholds come from:
  #     this paper that I think defines ponds as <2ha (<0.02sqkm) Williams, P. J., J. Biggs, A. Crowe, J. Murphy, P. Nicolet, A. Weatherby, and M. Dunbar. 2010b. CS Technical Report No. 7/07 Countryside Survey: Ponds Report from 2007. Lancaster.
  #     somewhat even thresholds
  # But as of 1/2/2026, custom thresholds can be used

  cat("Transforming\n")
  g1 <- st_transform(locs, st_crs(lakepond))

  cat("Intersecting\n")
  # Intersect national flowlines with grid
  tmp <- st_intersection(g1, lakepond) %>%
          mutate(area = st_area(.data$geometry)) %>%
          group_by(.data$id, .data$size) %>%
          summarize(area = as.numeric(sum(.data$area)), .groups = "drop", n = n()) %>%
          st_drop_geometry() %>%
          pivot_wider(names_from = "size",
                      values_from = c("area", "n"),
                      values_fill = 0)

  areacols <- paste0("area_", names(size))
  for (a in 1:length(areacols)) {
    if (areacols[a] %in% colnames(tmp) == F) {
      tmp[,areacols[a]] <- 0
    }
  }

  ncols <- paste0("n_", names(size))
  for (a in 1:length(ncols)) {
    if (ncols[a] %in% colnames(tmp) == F) {
      tmp[,ncols[a]] <- 0
    }
  }

  grid.covs <- g1 %>%
                st_drop_geometry() %>%
                full_join(tmp, by = "id") %>%
                #replace(is.na(.data), 0) %>%
                select(all_of(id.label), starts_with("area_"), starts_with("n_"))
  grid.covs[is.na(grid.covs)] <- 0

  return(grid.covs)

}
