################
#--- Step 7 ---#
################

#' Fuzzy supplementary poverty estimation.
#'
#' @description Constructs the fuzzy supplementary poverty measure based on Steps1-6.
#'
#' @param steps3_5 The results from previous steps.
#' @param weight A vector of sampling weights. If it is NULL (the default) weights are assigned assuming simple random sampling of units.
#' @param alpha The alfa parameter.
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha).
#' @param ... Other parameters.
#'
#' @return A list of results containing the fuzzy meambership function for each unit, the point estimate (i.e. the expected value of the function), and the alpha parameter.
#' @export
#'
#' @examples
#' fs_results = fs_construct(steps3_5 = steps3_5, weight = eusilc$DB090, alpha = alpha, breakdown = NULL)
#' fs_results = fs_construct(steps3_5 = steps3_5, weight = eusilc$DB090, alpha = alpha, breakdown = breakdown)

fs_construct <- function(steps3_5, weight, alpha, breakdown = NULL, ...){

  J <- max(steps3_5$Factor)

  res.list <-vector(mode = 'list', length = J+1)
  headers <- c(paste('Factor', 1:J), 'Overall')
  names(res.list) <- headers

  FS.data <- unique(steps3_5[,c('ID','s_i')])
  FS.data$weight <- weight # potrebbe essere meglio averla dallo step prima? altrimenti devo aggiungere di nuovo l'opzione in caso uno il peso non ce l'abbia. si il peso se non c'è lo attribuisco prima e me lo porto dietro sempre

  s <- FS.data[['s_i']]

  FS.data.ord <- FS.data %>% dplyr::arrange(s)
  s.ord <- FS.data.ord$s_i
  w.ord <- FS.data.ord$weight

  FS.data.ord$mu <- fs_mu(s.ord, w.ord, alpha)

  res.list[['Overall']] <- FS.data.ord

  for(j in 1:J){
    FS.data <- unique(steps3_5[steps3_5$Factor==j, c('ID','s_hi')])
    FS.data$weight <- weight # potrebbe essere meglio averla dallo step prima? altrimenti devo aggiungere di nuovo l'opzione in caso uno il peso non ce l'abbia

    s <- FS.data[['s_hi']]

    FS.data.ord <- FS.data %>% arrange(s)
    s.ord <- FS.data.ord[['s_hi']]
    w.ord <- FS.data.ord$weight

    FS.data.ord$mu <- fs_mu(s.ord, w.ord, alpha)
    res.list[[j]] <- FS.data.ord

  }
  estimate <- sapply(res.list, function(x) weighted.mean(x$mu, x$weight))

  if(!is.null(breakdown)){
    P <- length(unique(breakdown))
    estimate <- sapply(1:(J+1), function(j) sapply(split(res.list[[j]], breakdown), function(x) weighted.mean(x$mu, x$weight)))
    colnames(estimate) <- headers
  }


  return(list( membership = res.list, estimate = estimate, alpha = alpha))
}
