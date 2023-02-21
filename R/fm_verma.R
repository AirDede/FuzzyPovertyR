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
#'
#' @export
fm_verma <- function(monetary, weight, ID, HCR, interval, alpha, breakdown){ # cambiare ordine dei parametri

  N <- length(monetary)
  if(is.null(ID)) ID <- seq_len(N)
  fm_data <-  data.frame(ID = ID, monetary = monetary, weight = weight) %>% arrange(monetary)


  monetary.ord <- fm_data[['monetary']]
  weight.ord <- fm_data[['weight']] # actually ordered according to monetary

  if(is.null(alpha)){
    cat('Solving non linear equation: E[u] = HCR\n')
    alpha <- uniroot(fm_objective,
                     interval = interval,
                     monetary.ord = monetary.ord,
                     weight.ord = weight.ord,
                     HCR = HCR)$root
    cat('Done.\n')
  }

  fm_data$mu <- fm_mu(monetary.ord, weight.ord, alpha)
  estimate <- weighted.mean(fm_data$mu, fm_data$weight)

  if(!is.null(breakdown)){

    fm_data <- split(fm_data, breakdown)
    estimate <- sapply(fm_data, function(x) weighted.mean(x$mu, x$weight))

    # fa funzioni di appartenenza per ogni breakdown. per altro paper ;T

    # fm_data.list <- split(fm_data, breakdown) %>% lapply(function (x) x %>% arrange(monetary))
    # mu.list <- lapply(fm_data.list, function(x) fm_mu(x$monetary, x$weight, alpha))
    # fm_data.list <- Map(cbind, fm_data.list, mu.list); for(i in 1:length(fm_data.list)) colnames(fm_data.list[[i]]) <- c('ID', 'monetary','weight','membership')
    # fm_estimates <- sapply(fm_data.list, function(x) weighted.mean(x$membership, x$weight))
    # return(list(results = fm_data.list, estimates = fm_estimates, alpha = alpha))

  }
  out <- list(results = fm_data, estimate = estimate, alpha = alpha)
  return(out)

}
