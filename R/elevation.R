#' Elevation
#'
#' @description Process USA elevation data using data from the Commission for Environmental Cooperation (CEC). Before running this function, you must download the USA elevation data from \url{http://www.cec.org/north-american-environmental-atlas/elevation-2023/}. Unzip and call the path to the \emph{elevation_tif} folder in the function \code{get_elevation()} function.
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param path (character) Path to location of data to extract
#' @param id.label (character) Column name of location ID
#' @param method (character) Method to extract data using \pkg{terra}::\code{extract()}. Valid methods are "fast" and "precise"
#'
#' @returns A data frame with summarized elevation for each polygon in locs
#' @export
#'
#' @importFrom rlang .data
#' @importFrom tidyselect all_of
#' @importFrom terra zonal terrain crop rast extract
#' @importFrom dplyr rename group_by summarize filter mutate group_by select
#' @importFrom tidyr pivot_wider
#' @importFrom sf st_crs st_transform
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' get_elevation(locs, path = 'data/', id.label = 'grid.id')
#' }

get_elevation <- function(locs,
                          path,
                          id.label,
                          method = "fast") {

  if (method %in% c("precise", "fast") == F) {
    stop('"method" must be "precise" or "fast"')
  }

  tmp <- cleaning(locs, id.label, path)
  locs <- tmp$locs
  path <- tmp$path

  if ("elevation_tif" %in% list.files(path) == F) {
    stop("USA Elevation file does not exist. \nStep 1: Download TIF from http://www.cec.org/north-american-environmental-atlas/elevation-2023/ \nStep 2: Unzip, move elevation_tif folder to path, and try again")
  }


  # load data
  elev <- rast(paste0(path, "/elevation_tif/Elevation_TIF/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif"))

  # set up data
  locs1 <- st_transform(locs, st_crs(elev))


  bb <- get_buffered_bbox(locs1)
  elev1 <- crop(elev, bb)
  sub <- elev1



  if (is.na(sub@pntr$range_max)) next


  # Get elevation
  if (method == "fast") {
    ned <- zonal(sub,
                 vect(locs1),
                 na.rm = T) %>%
      rename(elevation = .data$northamerica_elevation_cec_2023)
  } else {
    ned <- zonal(sub,
                 vect(locs1),
                 na.rm = T,
                 weights = T) %>%
      rename(elevation = .data$northamerica_elevation_cec_2023)
  }



  # Get derived metrics
  vars <- c("slope", "aspect", "TRI", "flowdir")


  if (dim(sub)[1] < 5) next
  if (dim(sub)[2] < 5) next

  ind <- terrain(sub,
                 v = vars,
                 unit = "degrees",
                 neighbors = 4)

  if (method == "fast") {
    ind1 <- zonal(ind,
                  vect(locs1),
                  na.rm = T)
  } else {
    ind1 <- zonal(ind,
                  vect(locs1),
                  na.rm = T,
                  weights = T)
  }

  # Combine with elevation
  ned[2:5] <- ind1


  # Get aspect
  if (method == "fast") {
    ind.aspect <- extract(ind$aspect, vect(locs1)) %>%
      mutate(weight = 1)
  } else {
    ind.aspect <- extract(ind$aspect, vect(locs1), weights = T)
  }



  if (sum(ind.aspect$aspect, na.rm = T) != 0) {
    # # mode aspect of pixels (if there is a tie, get mean)
    # ind2 <-  ind.aspect %>%
    #   group_by(.data$ID) %>%
    #   summarize(metric = .getmode(.data$aspect, ties_method = "mean"))
    # 
    # ned$aspect.mode.mean <- ind2$metric
    # 
    # # mode aspect of pixels (if there is a tie, get random)
    # ind2 <- ind.aspect %>%
    #   group_by(.data$ID) %>%
    #   summarize(metric = .getmode(.data$aspect, ties_method = "random"))
    # 
    # if (nrow(ind2) != nrow(ned)) ned$aspect.mode.random <- NA
    # if (nrow(ind2) == nrow(ned)) ned$aspect.mode.random <- ind2$metric

    # % of pixels facing each direction
    dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "flat")

    indtmp <- filter(ind.aspect, is.na(.data$aspect) == F)
    ind2 <- mutate(ind.aspect, direction = case_when(aspect >= 0 & aspect < 22.5 ~ "N",
                                                     aspect >= 22.5 & aspect < 67.5 ~ "NE",
                                                     aspect >= 67.5 & aspect < 112.5 ~ "E",
                                                     aspect >= 112.5 & aspect < 157.5 ~ "SE",
                                                     aspect >= 157.5 & aspect < 202.5 ~ "S",
                                                     aspect >= 202.5 & aspect < 247.5 ~ "SW",
                                                     aspect >= 247.5 & aspect < 292.5 ~ "W",
                                                     aspect >= 292.5 & aspect < 337.5 ~ "NW",
                                                     aspect >= 337.5 ~ "N",
                                                     aspect < 0 ~ "flat")) %>%
                group_by(.data$ID, .data$direction) %>%
                summarize(metric = sum(.data$weight), .groups = "drop") %>%
                pivot_wider(values_from = .data$metric, names_from = .data$direction)
    ind2$`NA` <- NULL

    # make sure all directions are in df
    missing <- dirs[which(dirs %in% colnames(ind2) == F)]
    for (i in 1:length(missing)) {
      ind2[missing[i]] <- 0
    }


    ind2 <- select(ind2, "ID", all_of(dirs), "flat")

    # add to ned_grid
    ned[(ncol(ned)+1):(ncol(ned)+length(dirs))] <- ind2[2:ncol(ind2)]

  }

  ned$id <- locs1$id
  all <- select(ned, "id", everything())

  colnames(all)[1] <- id.label


  return(all)

}
