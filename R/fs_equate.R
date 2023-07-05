################
#--- Step 6 ---#
################

# steps4_5 <- result
#
#' Fuzzy supplementary poverty estimation
#'
#' @description This function solves $E(mu)^(alpha-1) = HCR$ for alpha.
#'
#' @param steps4_5 The results obtained from `fs_weight`.
#' @param weight A numeric vector of sampling weights. if NULL simple random sampling weights will be used
#' @param HCR The head count ratio.
#' @param interval The range to look for the value of alpha.
#'
#' @return The alpha parameter that solves the non-linear equation $E(mu) = HCR$
#' @export
#'
#' @examples
#' data(eusilc)
#' step2 = fs_transform(eusilc[,4:23], weight = eusilc$DB090, ID = eusilc$ID)
#' dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5)
#' steps4_5 = fs_weight(dimensions, step2 = step2, rho = NULL)
#' alpha <- fs_equate(steps4_5 = steps4_5, weight = eusilc$DB090, HCR = .16, interval = c(1,10))
fs_equate <- function(steps4_5, weight, HCR, interval = c(1,10) ){ # weight has to be attached to the data frame. should not be specified each time.

  if(is.null(weight)) weight <- nrow(steps4_5)
  FS.data <- unique(steps4_5[,c('ID','s_i')])
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
