###################
#--- Steps 4-5 ---#
###################

#' Fuzzy supplementary poverty estimation
#'
#' @description Calculates the weights of dimensions discovered after factor analysis.
#'
#' @details
#'
#' @param dimensions A numeric vector (of length  `ncol(data)`) of assignments of items in data to dimensions.
#' @param step2 The data frame resulting from step2.
#' @param rho The critical value to be used for calculation of weights in the kendall correlation matrix.
#'
#' @return A data frame of weights and deprivation scores in each dimension identified.
#' @export
#'
#' @example
#' dimensions = c(1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5)
#' steps4_5 = fs_weight(dimensions, step2 = step2, rho = NULL)

fs_weight <- function(dimensions, step2, rho = NULL){

  J <- max(dimensions) # number of identified dimensions
  cor.list <- vector(mode = "list", length = J)

  j <- 1:J

  wb.jh_list <- unlist( lapply( j, wb.jh, step2 = step2, dimensions = dimensions, rho) ) # per ogni dimensione calcolo il peso di ogni indicatore
  wb.jh_df <- data.frame(Item = names(wb.jh_list), w_b = wb.jh_list)

  Items <- colnames(step2)[-1] # elimino la colonna ID, si potrebbe evitare lo step?
  # calcolare i coefficienti di variazione degli (1-s)
  result <- step2 %>%
    reshape2::melt(id.vars = 'ID', variable.name = 'Item', value.name = 's') %>%
    dplyr::inner_join(data.frame(Item = Items, Factor = dimensions), by = 'Item') %>%
    dplyr::group_by(Factor, Item) %>% # raggruppo per item, dimensione
    dplyr::mutate(w_a = sd(s) / mean(s) ) %>%
    dplyr::inner_join(wb.jh_df) %>% # aggiungo i pesi dallo step prima
    dplyr::mutate(w = w_a*w_b) %>%
    dplyr::group_by(Factor, ID) %>%
    dplyr::mutate(s_hi = weighted.mean(s, w = w)) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(s_i =  mean(s_hi)) # CONTROLLARE FORMULA! CON P1080 BETTI EMPIRICAL ECONOMICS

  return(result)
}
