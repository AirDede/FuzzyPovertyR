#' Fuzzy monetary poverty estimation
#' 
#' @description This function calculates the fuzzy membership function as defined in Betti et. al, 2018.
#'
#' @param income.ord A sorted vector of equivalised disposable incomes (in ascending order).
#' @param weight.ord A sorted vector of weights (in the same order of s.ord)
#' @param alpha The value of the exponent parameter to use in the non-linear equation as of Betti et. al, 2018.
#'
#' @return A numeric vector containing the estimated membership function.
#'
#' @examples
fm_mu <- function(income.ord, weight.ord, alpha){
  
  N = length(income.ord)
  tot1 = sum(income.ord[2:N])
  tot2 = sum( (income.ord*weight.ord)[2:N] )
  
  F <- rep(0,N)
  L <- rep(0,N)
  
  cond <- income.ord > income.ord[1]
  tot1 <- sum( weight.ord*cond )
  tot2 <- sum( (income.ord*weight.ord)*cond )
  
  i <-  1
  while(i < N) {
    flag_i <- income.ord > income.ord[i]
    F[i] <- sum( weight.ord*flag_i ) / tot1
    L[i] <- sum( (income.ord*weight.ord)[ flag_i ]) / tot2
    i <- i + 1
  }
  
  u <- F^(alpha-1)*L
  return(u)
}
