#' Title
#'
#' @param v
#' @param ties_method
#'
#' @returns
#' @export
#'
#' @examples

.getmode <- function(v,
                     ties_method = "random") {

  tab <- table(v)
  m <- max(tab)

  modenums <- as.numeric(names(tab)[which(tab == m)])

  if (ties_method == "random") {

    if (length(modenums) > 1) {
      winner <- sample(modenums, 1)
    } else {
      winner <- modenums
    } }

  if (ties_method == "mean") {winner <- mean(modenums)}

  return(winner)

}
