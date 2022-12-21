#' Fuzzy supplementary poverty estimation.
#'
#' @param x 
#' @param y 
#'
#' @return the sum of x,y
#'
#' @examples
modifiedSum <- function(x, y) {
  
  replace(x, is.na(x), 0) + replace(y, is.na(y), 0)
}
