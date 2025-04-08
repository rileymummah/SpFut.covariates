#' Protected areas
#'
#' @description Process US protected area data. Before running this function, you must download \emph{PADUS4_0Geodatabase.zip} from \url{https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download}. Unzip and call the path to the \emph{PADUS4_0Geodatabase} folder in the \code{get_protected()} function.
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param path (character) Path to location of data to extract
#' @param id.label (character) Column name of location ID
#'
#' @returns A data frame with summarized protected areas for each polygon in locs
#' @export
#'
#' @importFrom tidyselect starts_with all_of
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_protected(locs, path = 'data/', id.label = 'grid.id')
#' }

get_protected <- function(locs,
                          path,
                          id.label = "id") {

  tmp <- cleaning(locs, id.label, path)
  locs <- tmp$locs
  path <- tmp$path

  if ("PADUS4_0Geodatabase" %in% list.files(path) == F) {
    stop("Protected areas file does not exist. \nStep 1: Download PADUS4_0Geodatabase.zip from https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download \nStep 2: Unzip, move PADUS4_0Geodatabase folder to path, and try again")
  }


  pad <- sf::st_read(paste0(path, "/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb/"),
                     layer='PADUS4_0Designation') %>%

        # Drop Z or M dimension
        sf::st_zm()

  # these are good
  valid <- pad[which(sf::st_is_valid(pad) == T),]

  # these are bad
  invalid <- pad[which(sf::st_is_valid(pad) == F),]
  invalid1 <- sf::st_make_valid(invalid)

  # these are very bad
  other <- pad[which(is.na(sf::st_is_valid(pad))),]
  # they are concentrated in the west but spread across CONUS
  #
  # tmp <- st_cast(other, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")
  # tmp <- st_cast(other, "MULTISTRING")
  # tmp <- st_as_sfc(other) %>% st_cast("MULTISTRING")
  #
  #
  # load("data/species/Arizona Toad/all-data.rdata")
  # ggplot() +
  #   geom_sf(data = region$conus) +
  #   geom_sf(data = other)
  #
  #other1 <- st_make_valid(other)
  #other1 <- st_cast(other, "GEOMETRYCOLLECTION")
  # DONT KNOW WHAT TO DO ABOUT THESE
  cat("Warning: There are ", nrow(other), " shapes that are not useable")

  # project locs
  g1 <- dplyr::select(locs, id, geometry) %>%
          sf::st_transform(sf::st_crs(pad))
  bb <- get_buffered_bbox(g1)

  #
  pad1 <- dplyr::bind_rows(valid, invalid1) %>% sf::st_crop(bb)

  if (nrow(pad1) == 0) {
    allnms <- paste0("protected.", c("FED", "LOC", "UNK", "STAT", "NGO", "DIST", "PVT", "JNT", "TRIB", "TOT"))

    g2 <- as.data.frame(matrix(ncol = length(allnms),
                               nrow = nrow(g1),
                               data = 0))
    colnames(g2) <- allnms

    g2$id <- g1$id

    g2 <- dplyr::select(g2, id, dplyr::all_of(allnms))

  } else {

    # combine each management type into one row
    pad1 <- dplyr::group_by(pad1, Mang_Type) %>%
            dplyr::summarize(geometry = sf::st_union(SHAPE))

    # get intersection
    tmp <- sf::st_intersection(g1, pad1)
    tmp$overlap.area <- as.numeric(sf::st_area(tmp))

    # add up protected area of each type within each cell
    tmp <- dplyr::group_by(tmp, id, Mang_Type) %>%
            dplyr::summarize(protected = sum(overlap.area),
                      .groups = "drop") %>%
            sf::st_drop_geometry()

    g2 <- dplyr::full_join(g1, as.data.frame(tmp), by = "id") %>%
            sf::st_drop_geometry() %>%
            tidyr::pivot_wider(names_from = Mang_Type, values_from = protected)

    g2$`NA` <- NULL

    colnames(g2)[2:ncol(g2)] <- paste0("protected.", colnames(g2)[2:ncol(g2)])

    g2[is.na(g2)] <- 0
    g2$protected.TOT <- rowSums(g2[,2:ncol(g2)])

    g2 <- locs %>%
            sf::st_drop_geometry() %>%
            #select(id) %>%
            dplyr::full_join(g2, by = "id")

    # make sure all columns exist
    allnms <- paste0("protected.", c("FED", "LOC", "UNK", "STAT", "NGO", "DIST", "PVT", "JNT", "TRIB", "TOT"))
    missingnms <- allnms[-which(allnms %in% colnames(g2))]

    if (length(missingnms) > 0) {
      for (m in 1:length(missingnms)) {
        g2[,(ncol(g2)+1)] <- NA
        colnames(g2)[ncol(g2)] <- missingnms[m]
      }
    }


    g2[is.na(g2)] <- 0

    g2 <- dplyr::select(g2, all_of(id.label), tidyselect::starts_with("protected."))


  }



  return(g2)

}
