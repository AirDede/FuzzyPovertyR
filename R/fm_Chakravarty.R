#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a poverty predicate
#' @param z The parameter of the f.m. function (see Chakravarty (2006))
#'
#' @return The membership grades
#' @export
#'
#' @examples
#' x = rchisq(1000, 15)
#' fm_Chakravarty(x, z = 10)
#' @references
#' Chakravarty, S. R. (2019). An axiomatic approach to multidimensional poverty measurement via fuzzy sets. Poverty, social exclusion and stochastic dominance, 123-141.
#'
fm_Chakravarty <- function(x, z){
  N <- length(x)
  y <- rep(NA, N)
  y[x == 0] <- 1
  y[0<=x & x < z] <- (z - x[0<=x & x < z])/z
  y[x>=z] <- 0
  return(y)
}
