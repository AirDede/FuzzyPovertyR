################
#--- Step 6 ---#
################

# steps3_5 <- result
# 
#' Fuzzy supplementary poverty estimation
#' 
#' @description This function solves $E(mu)^(\alpha-1) = HCR$ for alpha.
#'
#' @param steps3_5 The results obtained from `fs_weight`.
#' @param weight A vector of sampling weights. If it is NULL (the default) weights are assigned assuming simple random sampling of units.
#' @param HCR The head count ratio.
#' @param interval The range to look for the value of alpha.
#'
#' @return The alpha parameter that solves the non-linear equation $E[mu] = HCR$
#' @export
#'
#' @examples
fs_equate <- function(steps3_5, weight, HCR, interval = c(1,10) ){ # weight has to be attached to the data frame. should not be specified each time.
  
  if(is.null(weight)) weight <- nrow(steps3_5)
  FS.data <- unique(steps3_5[,c('ID','s_i')])
  FS.data$weight <- weight # potrebbe essere meglio averla dallo step prima? altrimenti devo aggiungere di nuovo l'opzione in caso uno il peso non ce l'abbia.
  
  s <- FS.data[['s_i']]
  
  FS.data.ord <- FS.data %>% dplyr::arrange(s)
  s.ord <- FS.data.ord$s_i
  w.ord <- FS.data.ord$weight
  
  alpha <- uniroot(objective, 
                   interval = interval,
                   s.ord = s.ord,
                   w.ord = w.ord,
                   HCR = HCR)$root
  cat('Done.')
  return(alpha)
  
}
