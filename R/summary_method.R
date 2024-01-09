
#' The summary of a FuzzyPoverty object
#' @description Summary method for class "FuzzyPoverty"
#'
#' @param An object of class "FuzzyPoverty"
#'
#' @return The summary method for class "FuzzyPoverty"
#' @export
#'
summary.FuzzyPoverty <- function(obj) {
  if(obj$fm!="ZBM") {
  cat("Fuzzy monetary results using", obj$fm, "membership function: \n\n",
      "Summary of the membership function \n\n",
      "quantiles\n",
      names(round(quantile(obj$results$mu, probs = c(0, .2, .4, .6, .8, 1)), digits = 4)), "\n",
      round(quantile(obj$results$mu, probs = c(0, .2, .4, .6, .8, 1)), digits = 3), "\n\n",
      "E[mu] = ", obj$estimate, "\n",
      "Parameter(s)", names(obj$parameters), ":", unlist(obj$parameters)
  )} else {
    cat("Fuzzy monetary results using", obj$fm, "membership function: \n\n",
        "Summary of the membership function \n\n",
        "mean  over the individual states \n",
        round(mean(rowMeans(obj$mu)), digits = 3), "\n\n",
        "E[mu] = ", obj$estimate, "\n",
        "Parameter(s)", names(obj$parameters), ":", unlist(obj$parameters)
    )
  }
}


