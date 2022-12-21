#' Fuzzy supplementary poverty estimation
#' 
#' @description The objective function to find the root of.
#'
#' @param s.ord A vector of ordered deprivation scores.
#' @param w.ord A vector of ordered sampling weights. In the same order of `s.ord`.
#' @param alpha The alpha parameter.
#' @param HCR The head count ratio.
#'
#' @return The difference between the expected value of the membership function and the head count ratio.
#'
#' @examples
objective <- function(s.ord, w.ord, alpha, HCR){
  FS <- fs_mu(s.ord, w.ord, alpha)
  print(paste0('trying with alpha: ', round(alpha, 4) , ' Expected Value: ', round(weighted.mean(x = FS, w = w.ord), 4))) 
  return( weighted.mean(x = FS, w = w.ord) - HCR )
}
