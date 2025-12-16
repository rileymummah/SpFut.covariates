#' Cleans locations, path, and polygon ID
#'
#' @param locs (sf) Polygons for which to summarize covariates (should be grid cells, watersheds, or buffered points)
#' @param id.label (character) Column name of location ID
#' @param path (character) Path to location of data to extract
#'
#' @returns A list of the locations and the path
#' @export
#'
#' @importFrom dplyr pull
#'
#' @examples
#' \dontrun{
#' data(locs)
#'
#' cleaning(locs, id.label = 'grid.id', path = 'data/')
#' }

cleaning <- function(locs, id.label, path) {

  id.ind <- grep(id.label, colnames(locs))
  locs$id <- pull(locs, id.label)

  if (substr(path, nchar(path), nchar(path)) == "/") path <- substr(path, 1, nchar(path)-1)

  dat <- list(locs = locs,
              path = path)
}
