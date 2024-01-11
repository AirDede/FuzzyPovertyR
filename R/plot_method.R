#' The plot of a FuzzyPoverty object
#' @description plot method for class "FuzzyPoverty"
#'
#' @param An object of class "FuzzyPoverty"
#' @import tidyr
#' @import ggplot2
#' @return The plot
#' @export
#'

plot.FuzzyPoverty <- function(obj,...){
  is_breakdown <- "breakdown" %in% colnames(obj$results)
  if(!is_breakdown){
    if(obj$fm == "verma") {
      FL.curve = fm_FL(obj$results$predicate, obj$results$weight)
      obj.plot.data = data.frame(obj$results, Lorenz = FL.curve$Lorenz, WECDF = FL.curve$WECDF)
      obj.plot.data %>% dplyr::select(-weight) %>%
        tidyr::pivot_longer(cols = c(mu, Lorenz, WECDF), names_to = "curve", values_to = "value") %>%
        ggplot2::ggplot(ggplot2::aes(x = predicate, y = value, linetype = curve)) +
        ggplot2::geom_line() +
        ggplot2::scale_linetype_manual(values = c("Lorenz" = "dashed", "mu" = "solid", "WECDF" = "dotted")) +
        ggplot2::geom_area(position = "identity", alpha = .1) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom")

    } else if (obj$fm == "Verma and Betti") {
      FL.curve = fm_FL(obj$results$predicate, obj$results$weight)
      obj.plot.data = data.frame(obj$results, Lorenz = FL.curve$Lorenz)
      obj.plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = predicate, y = Lorenz)) +
        ggplot2::geom_line() +
        ggplot2::geom_area(position = "identity", alpha = .1) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom")

    } else if (obj$fm == "Totally fuzzy and Relative") {
      FL.curve = fm_FL(obj$results$predicate, obj$results$weight)
      obj.plot.data = data.frame(obj$results, WECDF = FL.curve$WECDF)
      obj.plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = predicate, y = WECDF)) +
        ggplot2::geom_line() +
        ggplot2::geom_area(position = "identity", alpha = .1) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom")
    } else {
      paste(obj$fm)
    }
  } else {
    if(fm = "verma"){
      # lapply(split(obj$results, f = obj$results$breakdown), function(x) fm_FL(x$predicate, x$weight))
      obj$results %>% ggplot2::ggplot(ggplot2::aes(x = predicate, y = mu)) +
        ggplot2::geom_line() + ggplot2::geom_area(alpha = .2) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::facet_wrap(~breakdown, scales = "free_x") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom",
                       axis.text.x = element_text(angle = 90))

    }
  }
}
