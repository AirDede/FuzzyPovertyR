#' Fuzzy monetary poverty estimation
#'
#' @description This function estimates the variance of the fuzzy monetary poverty index
#'
#' @param monetary A numeric vector of a monetary variable (i.e. equivalised income or expenditure)
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used.
#' @param ID A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha). Ff numeric will be coherced to a factor.
#' @param type The variance estimation method chosen. One between `bootstrap` (default) or `jackknife`.
#' @param R The number of bootstrap replicates. Default is 500.
#' @param M The size of bootstrap samples. Default is `nrow(data)`.
#' @param stratum The vector identifying the stratum (if 'jackknife' is chosen as variance estimation technique).
#' @param psu The vector identifying the psu (if 'jacknife' is chosen as variance estimation technique).
#' @param f The finite population correction fraction (if 'jackknife' is chosen as variance estimation technique).
#' @param verbose Logical. whether to print the proceeding of the variance estimation procedure.
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
#' @return The estimate of variance with the method selected. if breakdown is not NULL, the variance is estimated for each sub-domain.
#' @export
#' @examples
#' data(eusilc)
#' HCR <- 0.14
#' hh.size <- rep(1, 1000)
#' fm_var(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "bootstrap", HCR = .14, alpha = 9)
#' fm_var(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "verma", breakdown = eusilc$db040, type = "jackknife", HCR = .14, alpha = 9, stratum = eusilc$stratum, psu = eusilc$psu)
#' fm_var(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "ZBM", breakdown = eusilc$db040, type = "bootstrap", hh.size = hh.size, K = 3)
#' fm_var(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", breakdown = eusilc$db040, type = "bootstrap", z1 = 1000, z2 = 2000, b = 2)
#' fm_var(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "belhadj", breakdown = eusilc$db040, type = "jackknife", z1 = 1000, z2 = 2000, b = 2, stratum = eusilc$stratum, psu = eusilc$psu)
#' fm_var(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "chakravarty", breakdown = eusilc$db040, type = "bootstrap", z = 2000)
#' fm_var(monetary = eusilc$red_eq, weight = eusilc$DB090, fm = "chakravarty", breakdown = eusilc$db040, type = "jackknife", z = 2000, stratum = eusilc$stratum, psu = eusilc$psu)

fm_var <- function(monetary, weight, fm, ID = NULL,
                   breakdown = NULL, type = 'bootstrap',
                   R = 100, M = NULL,
                   stratum, psu, f = 0.01,
                   verbose = TRUE,
                   HCR, interval = c(1,10), alpha = NULL,
                   hh.size, K,
                   z1, z2, b,
                   z) {

  N <- length(monetary)
  if(is.null(weight)) weight <- N
  if(is.null(ID)) ID <- seq_len(N)
  if(is.null(M)) M <- N
  if(!is.null(breakdown)) breakdown <- as.factor(breakdown)
  switch(type, # creare funzione bootstrap e funzione jacknife da chiamare qui invece che codificarle
         bootstrap = {
           BootDistr <- sapply(1:R, function(x) {
             if(verbose == T) cat('Bootstrap Replicate : ', x, 'of', R, '\n')
             bootidx <- sample(1:N, size = M, replace = T)
             ID.boot <- ID[bootidx]
             monetary.boot <- monetary[bootidx]
             weight.boot <- weight[bootidx]
             if(!is.null(breakdown)) breakdown <- breakdown[bootidx]
             if(fm=="ZBM") hh.size.boot <- hh.size[bootidx]
             try(fm_construct(monetary.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size.boot, K , z1, z2, b, z, breakdown)$estimate)
             # if(!is.null(breakdown)) {
             #   breakdown.boot <- breakdown[bootidx]
             #   try(fm_construct(monetary.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size[bootidx], breakdown.boot, z)$estimate)
             #   # var.hat <- apply(bootstrap.bill, 1, var)
             # } else {
             #   try(fm_construct(monetary.boot, weight.boot, fm, ID.boot, HCR, interval, alpha, hh.size[bootidx], breakdown, z)$estimate)
             #   # var.hat <- var(bootstrap.bill)
             # }
           }, simplify = "array")
           if(!is.null(breakdown)){
               if (fm=="ZBM") {
               # bootDistr.idx <- sapply(BootDistr, function(x) dim(x)[2]==k) # keep only
               # BootDistr <- BootDistr[bootDistr.idx]
               var.hat <- apply(BootDistr, 1:2, var) # verificare che sia corretta
               } else {

               var.hat <- apply(BootDistr, 1, var)
             }

           } else {
             var.hat <- var(BootDistr)
             # mettere un if per belhadj (ma belhadj ?? multidimensionale per me forse va meglio sotto FS)
           }
           # BootDistr <- sapply(mu.boot, mean) # fm_estimate for each replicate (meaningful only for breakdown?)
           # hist(BootDistr, xlab = '', main = expression(paste("Bootstrap distribution of E(", mu, ')')), probability = T)
           # var(BootDistr) #
         },
         jackknife = {
           tab <- data.frame(table(stratum, psu))
           a <- rowSums(with(tab, table(stratum, psu)))
           if(any(a<2)) stop("There should be at least 2 PSUs in each stratum")
           strata <- unique(stratum)
           H <- length(strata) # can be also a character string then
           w_jh <- tapply(weight, list(stratum, psu), sum, na.rm = T) # sum of the weights inside the strata
           w_h <- rowSums(w_jh) # sum of the weights inside the strata
           z_h = vector(mode = 'list', length = H)
           var_h <- rep(0, H)
           if(!is.null(breakdown)) var_h <- matrix(0, nrow = H, ncol = length(unique(breakdown)), dimnames = list(NULL, levels(breakdown)))
           for(h in 1:H){
             if(verbose == T) cat('doing for stratum',h,'of',H,'\n')
             stratum_h <- strata[h]
             psu_h <- tab$psu[tab$stratum==stratum_h] # psu-s in statum h
             a_h <- length(psu_h)
             z_hi <- g_hi <- rep(0, a_h)
             if(!is.null(breakdown)) z_hi <- vector(mode = 'list', length = a_h)
             for(i in 1:a_h){
               if(verbose == T) cat('doing for psu', i, 'of',a_h,'\n')
               psu_jh <- psu_h[i]
               delete.idx <- !(stratum==stratum_h & psu==psu_jh) # eliminating observations in psu
               # change the weights
               case1.idx <- (stratum!=stratum_h)
               case2.idx <- (stratum==stratum_h & psu!=psu_jh)
               # case3 set to 0
               w <- rep(0,N)
               w[case1.idx] <- weight[case1.idx]
               g_hi[i] <- w_h[stratum_h]/(w_h[stratum_h] - w_jh[stratum_h, psu_jh])
               w[case2.idx] <- (weight*g_hi[i])[case2.idx]

               if(!is.null(breakdown)){
                 z_hi[[i]] <- fm_construct(monetary[delete.idx], weight[delete.idx], fm, ID[delete.idx], HCR, interval, alpha, hh.size, K , z1, z2, b, z, breakdown[delete.idx])$estimate

               } else {
                 z_hi[i] <- fm_construct(monetary[delete.idx], weight[delete.idx], fm, ID[delete.idx], HCR, interval, alpha, hh.size, K , z1, z2, b, z)$estimate

               }
             }

             if(!is.null(breakdown)){

               z_hi.breakdown <- do.call(rbind, z_hi)
               means.breakdown <- rep(1, a_h)%*%t(apply(z_hi.breakdown,2,mean))
               var_h.breakdown <- (1-f)*t(g_hi)%*%(z_hi.breakdown - means.breakdown)^2
               var_h[h,] <- var_h.breakdown

             } else {
               var_h[h] <- (1-f)*sum(g_hi*(z_hi - mean(z_hi))^2)
             }

           }
           if(!is.null(breakdown)) {
             var.hat <- list(estimate = apply(var_h, 2, sum) )
           } else {
             var.hat <- list(estimate = sum(var_h))

           }
         })
  var.hat
}

bootstrap.bill <- function(R, M, monetary, weight, ID, breakdown = NULL, verbose = T, ...){
  BootDistr <- sapply(1:R, function(x) {
    if(verbose == T) cat('Bootstrap Replicate : ', x, 'of', R, '\n')
    bootidx <- sample(1:N, size = M, replace = T)
    ID.boot <- ID[bootidx]
    monetary.boot <- monetary[bootidx]
    weight.boot <- weight[bootidx]
    if(!is.null(breakdown)) {
      breakdown.boot <- breakdown[bootidx]
      try(fm_construct(monetary.boot, weight.boot, ID.boot, HCR, interval, alpha, breakdown.boot)$estimate)
      # var.hat <- apply(bootstrap.bill, 1, var)
    } else {
      try(fm_construct(monetary.boot, weight.boot, ID.boot, HCR, interval, alpha, breakdown)$estimate)
      # var.hat <- var(bootstrap.bill)
    }
  })
  return(BootDistr)
}
