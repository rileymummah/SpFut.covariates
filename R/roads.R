#' Roads
#'
#' @description Process US road data. Before running this function, you must download from \href{https://geodata.bts.gov/datasets/0b6c2fd2e3ac40a7929cdff1d4cf604a_0/explore}{Transportation.gov}. Unzip, rename the folder \emph{roads.gdb}, and call the path to the folder in the \code{get_roads()} function.
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param path (character) Path to location of data to extract
#' @param id.label (character) Column name of location ID
#'
#' @returns A data frame with summarized roads for each polygon in locs
#' @export
#'
#' @importFrom tidyselect all_of
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_roads(locs, path = 'data/', id.label = 'grid.id')
#' }


get_roads <- function(locs,
                      path,
                      id.label) {

  tmp <- cleaning(locs, id.label, path)
  locs <- tmp$locs
  path <- tmp$path

  if ("roads.gdb" %in% list.files(path) == F) {
    stop("USA Roads file does not exist. \nStep 1: Download from https://geodata.bts.gov/datasets/0b6c2fd2e3ac40a7929cdff1d4cf604a_0/explore?location=23.778950%2C70.504834%2C3.54 \nStep 2: Unzip, rename the folder 'roads.gdb', move to path, and try again")
  }

  if ("roads.rds" %in% list.files(paste0(path, "/roads.gdb/")) == F) {
    cat("Pre-processing of roads data has not been done yet. Pre-processing now\n")


    state <- sf::st_layers(paste0(path, '/roads.gdb'))$name


    tmp <- sf::st_read(paste0(path, "/roads.gdb"), layer = state) %>%
            # Drop Z or M dimension
            sf::st_zm() %>%
            # Transform CRS from 4269 to 3857
            sf::st_transform(3857) %>%
            dplyr::rename(state = JURISNAME)

    saveRDS(tmp, file = paste0(path, '/roads.gdb/roads.rds'))

  }

  roads <- readRDS(paste0(path, "/roads.gdb/roads.rds"))

  g1 <- sf::st_transform(locs, sf::st_crs(roads))


  # Intersect roads with grid
  sf::st_intersection(g1, roads) %>%
    # Calculate road length per grid and change to numeric
    dplyr::mutate(roadLength.m = sf::st_length(.),
                  roadLength.m = as.numeric(roadLength.m)) %>%
    dplyr::group_by(id) %>%
    # By grid, calculate the total road length (km)
    dplyr::summarize(roadLength.km = sum(roadLength.m, na.rm=T)/1000) %>%
    # Drop the flowline geometry
    sf::st_drop_geometry() %>%
    dplyr::select(id, roadLength.km) -> grid.covs


  # Join with locs
  roads1 <- locs %>%
              sf::st_drop_geometry() %>%
              #select(id) %>%
              dplyr::full_join(grid.covs, by = "id")
  roads1[is.na(roads1)] <- 0

  roads2 <- dplyr::select(roads1, all_of(id.label), roadLength.km)

  return(roads2)

}
