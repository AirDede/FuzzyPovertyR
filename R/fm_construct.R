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
#' @param fm The memebership function (deafult is "verma". Other options are "ZBM", "belhadj", "chakravarty". See references below.)
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha).
#' @param HCR If fm="verma". The value of the head count ratio.
#' @param interval If fm="verma". A numeric vector of length two to look for the value of alpha (if not supplied).
#' @param alpha If fm="verma". The value of the exponent in equation $E(mu)^(\alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR
#' @param hh.size If fm="ZBM". A numeric vector of household size.
#' @param k If fm="ZBM". The number of change points locations to estimate.
#' @param z1 If fm="belhadj".
#' @param z2 If fm="belhadj".
#' @param b If fm="belhadj". The shape parameter (if b=1 the mf is linear between z1 and z2)
#' @param z If fm="chakravarty".
#'
#' @return
#' If fm="verma". It returns a list containing the (fuzzy) membership function for each individual in the sample,
#' the estimated expected value of the function, the alpha parameter. if breakdown is supplied it gives an output for each level.
#'
#' @examples
#' data(eusilc)
#' HCR <- .154
#' hh.size <- sample(1:4, 1000, replace = T)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "verma", HCR = HCR, ID = eusilc$ID)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, HCR = HCR, ID = eusilc$ID, breakdown = eusilc$db040)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "ZBM", hh.size = hh.size, K = 3)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, breakdown = eusilc$db040, fm = "chakravarty", z = 1)
#' fm_construct(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", breakdown = eusilc$db040, z1 = 1000, z2 = 2000, b = 2)

#' @export
fm_construct <- function(monetary, weight, fm = "verma", ID = NULL,
                         HCR, interval = c(1,10), alpha = NULL,
                         hh.size, k=2,
                         z1, z2, b,
                         z,
                         breakdown = NULL){ # cambiare ordine dei parametri

  switch(fm,
         verma = {res <- fm_verma(monetary, weight, ID, HCR, interval, alpha, breakdown)},
         ZBM = {res <- fm_ZBM(monetary, hh.size, weight, breakdown, k)},
         belhadj = {res <- fm_belhadj2015(monetary, z1, z2, b, breakdown, weight)},
         chakravarty = {res <- fm_Chakravarty(monetary, z, breakdown)})
  return(res)

}
