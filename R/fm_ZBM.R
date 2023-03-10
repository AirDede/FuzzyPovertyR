#' Fuzzy monetary poverty estimation
#'
#' @description Calculates bootstrap percentiles from Zedini and Belhadj (2015)
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param R The number of bootstrap replicates (dafaults to 500)
#'
#' @return A matrix of bootrstapped percentiles
#'
#' @references
#' Zedini, A., & Belhadj, B. (2015). A New Approach to Unidimensional Poverty Analysis: Application to the T unisian Case. Review of Income and Wealth, 61(3), 465-476.

bootP <- function(x, R = 500){
  M <- length(x)
  bootReps <- replicate(R, sample(x, size = M, replace = TRUE))
  boot.p <- apply(bootReps, 2, quantile, probs = seq(0.01, 1, 0.01))
  P <- apply(boot.p, 1, mean)
  return(P)
}

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param a Membership function parameter
#' @param b Membership function parameter
#' @param c Membership function parameter
#'
#' @return The memebrship grade
#'
#' @references
#' Zedini, A., & Belhadj, B. (2015). A New Approach to Unidimensional Poverty Analysis: Application to the T unisian Case. Review of Income and Wealth, 61(3), 465-476.

FN <- function(x, a, b, c){
  y <- rep(NA, length(x))
  y[x<a | x >= c] <- 0
  y[a<=x & x<= b] <- 1
  y[b<=x & x < c] = x[b<=x & x<c]*(-1/(c-b))+(c/(c-b))
  return(y)
}

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param P A matrix of bootrstapped percentiles
#'
#' @return The membership grade matrix

MemberhsipGradesMatrix <- function(x, P){
  n <- length(x)
  MGM <- matrix(0, nrow = n, ncol = 1)
  MGM[,1] <- FN(x = x, a = 0, b = P[1], c = P[2])
  MGM <- cbind(MGM, do.call(cbind, lapply(2:99, function(j) FN(x = x, a = P[j-1], b = P[j], c = P[j+1]))))
  MGM <- cbind(MGM, FN(x = x, a = P[99], b = 0.5*(P[99] + P[100]), c = P[100]))
  return(t(MGM))
}

#' Fuzzy monetary poverty estimation
#'
#' @param monetary A numeric vector of a monetary variable (or poverty predicate)
#' @param hh.size A numeric vector with the size of the houshold
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used
#' @param k The number of change points locations to estimate
#' @return The membership grades for each poverty state
#'
#' @examples
#' # three poverty states
#' x1 = rchisq(500, 15)
#' x2 = rchisq(500, 10)
#' x3 = rchisq(500, 30)
#' x = c(x1, x2, x3)
#' w = sample(1:5, 1500, replace = T) # household sizes
#' breakdown = sample(letters[1:3], size = 1500, replace = T,  prob = c(3,1,1))
#'
#' P <- bootP(x)
#' MGM <- MemberhsipGradesMatrix(x, P)
#' fm_ZBM(monetary = x, hh.size = w, weight = NULL, K = 3, breakdown = breakdown)
#'
#' @references
#' Zedini, A., & Belhadj, B. (2015). A New Approach to Unidimensional Poverty Analysis: Application to the Tunisian Case. Review of Income and Wealth, 61(3), 465-476.
#' Belhadj, B., & Matoussi, M. S. (2010). Poverty in tunisia: A fuzzy measurement approach. Swiss Journal of Economics and Statistics, 146(2), 431-450.

fm_ZBM <- function(monetary, hh.size, weight, breakdown, K){
  # Zendini, Belhadi, Matoussi
  N <- length(monetary)
  if(is.null(weight)) weight <- rep(1/N, N)
  if(is.null(hh.size)) hh.size <- rep(1, N)

  #--- Step 0 ---#
  P <- bootP(x = monetary) # bootstrap estimates of percentiles
  MGM <- MemberhsipGradesMatrix(x = monetary, P = P)
  #--- Step 1 ---#
  fuzzy.states <- ecp::e.divisive(MGM, sig.lvl=.05, R=199, k=K, min.size=30, alpha=1)
  # k <- fuzzy.states$k.hat
  # states.idx <- fuzzy.states$estimates
  clusters.idx <- fuzzy.states$cluster
  #--- Step 2-3 ---#
  step2_3 <- apply(MGM, 2, function(x) tapply(x, INDEX = clusters.idx, max))
  #--- Step 4 ---#
  Q <- apply(step2_3, 1, function(y) weighted.mean(x = y*hh.size, w = weight)) # change with sampling version if needed

  if(!is.null(breakdown)) {
    states.split <- apply(step2_3, 1,
               function(y) data.frame(y, hh.size, weight, breakdown)) # change with sampling version if needed
    Q <- lapply(states.split, function(X) tapply(X$y, INDEX = X$breakdown, weighted.mean, x = X$y*X$hh.size, w = X$weight))
  }
  return(list(estimate = Q, Membership = MGM))
}
