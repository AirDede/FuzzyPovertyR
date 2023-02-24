#' Fuzzy monetary poverty estimation
#'
#' @description
#' `fm_construct` constructs fuzzy monetary poverty estimates.
#'
#' @details
#' It implements the fuzzy set approach to monetary poverty measurement where
#' the usual dichotomy poor (1) not-poor(0) is replaced with a continuum score in $(0,1)$
#'
#' @param monetary A numeric vector of a monetary variable (i.e. equivalised income or expenditure)
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#' @param HCR The value of the head count ratio (this is not used in the case that alpha is supplied by the user).
#' @param interval A numeric vector of length two to look for the value of alpha (if not supplied).
#' @param alpha The value of the exponent in equation $E(mu)^(\alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha).
#'
#' @return
#' A list containing the (fuzzy) membership function for each individual in the sample, the estimated expected value of the function, the alpha parameter.
#'
#' @examples
#' data(eusilc)
#' HCR <- .154
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040, alpha = 2)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040, fm = "ZBM")$estimate
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, fm = "ZBM")$estimate
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040, fm = "chakravarty", z = 1)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040, fm = "belhadj", z1 = 10, z2 = 20, b = 2, z = 1)

#' @export
fm_construct <- function(monetary, weight, fm = "verma", ID = NULL, HCR, #spostare dove ci sono i parametri da mettere
                         interval = c(1,10), alpha = NULL,
                         hh.size = NULL, breakdown = NULL,
                         z1, z2, b,
                         z, k){ # cambiare ordine dei parametri

  switch(fm,
         verma = {res <- fm_verma(monetary, weight, ID, HCR, interval, alpha, breakdown)},
         ZBM = {res <- fm_ZBM(monetary, hh.size, weight, breakdown)},
         belhadj = {res <- fm_belhadj2015(monetary, z1, z2, b, breakdown, weight)},
         chakravarty = {res <- fm_Chakravarty(monetary, z, breakdown)})
  return(res)

}
