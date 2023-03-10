#' Fuzzy monetary poverty estimation.
#'
#' @param monetary.ord A sorted vector of a monetary variable (in ascending order).
#' @param weight.ord A sorted vector of weights (in the same order of s.ord)
#' @param alpha The value of the exponent parameter to use in the non-linear equation as of Betti et. al, 2018.
#' @param HCR The head count ratio.
#'
#' @return The value of the objective function
#'
#' @examples
fm_objective <- function(monetary.ord, weight.ord, alpha, HCR){

  FM <- fm_mu(monetary.ord, weight.ord, alpha)
  print(paste0('trying with alpha: ', round(alpha, 4) , ' Expected Value: ', round(weighted.mean(x = FM, w = weight.ord), 4)))
  return( weighted.mean(x = FM, w = weight.ord) - HCR )
}
