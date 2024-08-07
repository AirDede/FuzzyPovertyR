# Fuzzy monetary poverty estimation
#
# @param x A numeric vector of a poverty predicate
# @param z The parameter of the f.m. function (see Chakravarty (2006))
# @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
# @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha).
# @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#
# @return The membership grades
#
# @examples
# x = rchisq(1000, 15)
# breakdown = sample(letters, size = length(x), replace = TRUE )
# fm_construct(predicate = x, weight = NULL, breakdown = breakdown, fm = "chakravarty", z = 10)
#
# @references
# Chakravarty, S. R. (2019). An axiomatic approach to multidimensional poverty measurement via fuzzy sets. Poverty, social exclusion and stochastic dominance, 123-141.
#
fm_Chakravarty <- function(x, z, weight, breakdown, ID){
  if(z < min(x) | z > max(x)) stop("The value of z has to be between the minimum and the maximum of the predicate")
  N <- length(x)
  if(is.null(ID)) ID <- seq_len(N)
  y <- rep(NA_real_, N)
  y[x == 0] <- 1
  y[0<=x & x < z] <- (z - x[0<=x & x < z])/z
  y[x>=z] <- 0

  if(!is.null(breakdown)){
  estimate <- sapply(split(data.frame(y, weight, breakdown), f = ~ breakdown), function(X) weighted.mean(X[["y"]], w = X[["weight"]]))
  } else {
    estimate <- weighted.mean(x = y, w = weight)
  }
  fm_data <- data.frame(ID = ID, predicate = x, weight = weight, mu = y)
  fm_data <- fm_data[order(fm_data$mu),]
  return(list(results = fm_data,
              estimate = estimate,
              parameters = list(z = z),
              fm = "chakravarty"))
}

