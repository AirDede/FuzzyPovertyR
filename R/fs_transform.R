################
#--- Step 2 ---#
################

#' Fuzzy supplementary poverty estimation
#'
#' @description This function maps a set of answers to binary or categorical items to the (0,1) interval.
#'
#' @details
#' The function calculates deprivation score for item j and ind. i as $d_{j,i} = (1-F(c_{j,i})) / (1-F(1)) where F(c_{j,i})$ is the value of j-th item cumulation function for the i individual.
#' To obtain consistent measures of supplementary poverty it is important that items are in the right order.
#' Lower levels of the items have to correspond to more deprivation while higher levels of the items to a less deprivation.
#' Example:
#'
#' @param data A matrix or a data frame of identified items (see Step 1 of Betti et. al, 2018)
#' @param weight A vector of sampling weights. If it is NULL (the default) weights are assigned assuming simple random sampling of units.
#' @param ID A vector of length `nrow(data)` containing individuals IDs. if NULL (the default) row numbers will be used.
#' @param ... other parameters
#'
#' @return a matrix of the same dimension of `data` with items mapped into the (0,1) interval
#' @export
#'
#' @examples
#'
#' @references
#' Betti, G., Gagliardi, F., Lemmi, A., & Verma, V. (2015). Comparative measures of multidimensional deprivation in the European Union. Empirical Economics, 49(3), 1071-1100.
#'
#' Betti, G., Gagliardi, F., & Verma, V. (2018). Simplified Jackknife variance estimates for fuzzy measures of multidimensional poverty. International Statistical Review, 86(1), 68-86.
#'
fs_transform = function(data, weight = NULL, ID = NULL, ...) {
  N <- nrow(data)
  if(is.null(ID)) ID <- seq_len(N)
  if(is.null(weight)) weight <- rep(N,N)
  deprivation_scores <- apply(data, 2, fuzzyScaleItem, weight, ID)
  transformed_items <- data.frame( ID, do.call(cbind, lapply(deprivation_scores, function(x) x[['s']]) ), row.names = ID )
  return(transformed_items)

}
