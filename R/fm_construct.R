#' Fuzzy monetary poverty estimation
#'
#' @description
#' `fm_construct` constructs fuzzy monetary poverty estimates.
#'
#' @details
#' It implements the fuzzy set approach to monetary poverty measurement where
#' the usual dichotomy poor (1) not-poor(0) is replaced with a continuum score in $(0,1)$
#'
#' @param predicate A numeric vector representing the poverty predicate (i.e. income or expenditure)
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param fm The memebership function (deafult is "verma". Other options are "ZBM", "belhadj", "chakravarty", "cerioli", "verma1999" and "TFR". See Betti et. al (2023) The fuzzy approach to poverty measurement. Research handbook of measuring poverty and depreivation (ed. by J. Silber))
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#' @param HCR If fm="verma" or fm="verma1999" or fm="TFR" . The value of the head count ratio.
#' @param interval If fm="verma" or fm="verma1999" or fm="TFR". A numeric vector of length two to look for the value of alpha (if not supplied).
#' @param alpha If fm="verma" or fm="verma1999" or fm="TFR". The value of the exponent in equation $E(mu)^(alpha-1) = HCR$. If NULL it is calculated so that it equates the expectation of the membership function to HCR
#' @param hh.size If fm="ZBM". A numeric vector of household size.
#' @param k If fm="ZBM". The number of change points locations to estimate.
#' @param z1 A parameter of the membership function if fm="belhadj" or fm="cerioli"
#' @param z2 A parameter of the membership function if fm="belhadj" or fm="cerioli"
#' @param b A parameter of the membership function if fm="belhadj". The shape parameter (if b=1 the mf is linear between z1 and z2)
#' @param z A parameter of the membership function if fm="chakravarty".
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha).
#' @param verbose Logical. whether to print the proceeding of the procedure.
#'
#' @import dplyr
#'
#' @return
#' a list containing the (fuzzy) membership function for each individual in the sample,
#' the estimated expected value (`estimate`) of the function and the parameters of the
#' membership functions (supplied or calculated). if breakdown is supplied it gives an output for each level.
#'
#' @examples
#' data(eusilc)
#' HCR <- .154
#' hh.size <- sample(1:4, 1000, replace = TRUE)
#' fm_construct(predicate = eusilc$red_eq, weight = eusilc$DB090,
#' fm = "verma", HCR = HCR, ID = eusilc$ID)

#' @export
fm_construct <- function(predicate, weight, fm = "verma", ID = NULL,
                         HCR, interval = c(1,10), alpha = NULL,
                         hh.size, k=3,
                         z1, z2, b,
                         z,
                         breakdown = NULL,
                         verbose = TRUE){ # cambiare ordine dei parametri
  N <- length(predicate)
  if(is.null(weight)) weight <- rep(N, N)
  if(!!(fm %in% c("verma", "verma1999", "chakravarty", "cerioli", "TFR", "ZBM"))) stop(cat("Select a membership function from the list: \n ", c('fm="verma"\n', 'fm="verma1999"\n', 'fm="chakravarty"\n', 'fm="cerioli"\n', 'fm="TFR"\n', 'fm="ZBM"\n')))
  switch(fm,
         verma = {res <- fm_verma(predicate, weight, ID, HCR, interval, alpha, breakdown, verbose)},
         ZBM = {res <- fm_ZBM(predicate, hh.size, weight, breakdown, k)},
         belhadj = {res <- fm_belhadj2015(predicate, z1, z2, b, breakdown, weight)},
         chakravarty = {res <- fm_Chakravarty(predicate, z, weight, breakdown)},
         cerioli = {res <- fm_cerioli(predicate, z1, z2, weight, breakdown)},
         TFR = {res <- fm_TFR(predicate, weight, ID, HCR, interval, alpha, breakdown, verbose)},
         verma1999 = {res <- fm_verma2(predicate, weight, ID, HCR, interval, alpha, breakdown, verbose)})
  return(res)
}
