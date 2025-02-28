#' .getmode
#'
#' @param v input
#' @param ties_method (character) Method for breaking ties. Must be "random" or "mean"
#'
#' @returns A summarization with ties broken


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
