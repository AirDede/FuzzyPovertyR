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
#' fm_construct(monetary = x, weight, breakdown = breakdown, fm = "chakravarty", z = 10)
#'
#' @references
#' Chakravarty, S. R. (2019). An axiomatic approach to multidimensional poverty measurement via fuzzy sets. Poverty, social exclusion and stochastic dominance, 123-141.
#'
fm_Chakravarty <- function(x, z, weight, breakdown){
  N <- length(x)
  y <- rep(NA, N)
  y[x == 0] <- 1
  y[0<=x & x < z] <- (z - x[0<=x & x < z])/z
  y[x>=z] <- 0

  if(!is.null(breakdown)){
  estimate <- sapply(split(data.frame(y, weight), f = breakdown), function(X) weighted.mean(X[["y"]], w = X[["weight"]]))
  } else {
    estimate <- weighted.mean(x = y, w = weight)
  }
  return(list(mu = y, estimate = estimate))
}
