#' Fuzzy monetary poverty estimation
#'
#' @description
#' Solves the non-linear equation in Betti et. al, 2018.
#'
#' @details
#' Calculates the exponent parameter alpha of the non-linear equation of Betti et al, 2018 so that
#' the expected value of the fuzzy membership function equated the head count ratio.
#'
#' @param s.ord. a sorted vector of deprivation scores.
#' @param w.ord a sorted vector of weights (in the same order of s.ord)
#' @param interval. The interval to look for the solution of the equation.
#'
#' @return the obtained exponent alfa

fm_equate <- function(monetary.ord, weight.ord, interval){

  alpha <- uniroot(fm_objective,
                   interval = interval,
                   monetary.ord = monetary.ord,
                   weight.ord = weight.ord,
                   HCR = HCR)$root
  cat('Done.\n')
  return(alpha)
}
