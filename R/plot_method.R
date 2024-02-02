#' The plot of a FuzzyPoverty object
#' @description plot method for class "FuzzyPoverty"
#'
#' @param x An object of class "FuzzyPoverty"
#' @param ... Additional options
#' @import tidyr
#' @import ggplot2
#' @return The plot
#' @export
#'

plot.FuzzyPoverty <- function(x,...){
#WECDF<--WECDF
    if(x$fm == "verma") {
      FL.curve = fm_FL(x$results$predicate, x$results$weight)
      x.plot.data = data.frame(x$results, Lorenz = FL.curve$Lorenz, "WECDF" = FL.curve$WECDF)
      x.plot.data %>% dplyr::select(-weight) %>%
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

    } else if (x$fm == "Verma and Betti") {
      FL.curve = fm_FL(x$results$predicate, x$results$weight)
      x.plot.data = data.frame(x$results, Lorenz = FL.curve$Lorenz)
      x.plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = predicate, y = Lorenz)) +
        ggplot2::geom_line() +
        ggplot2::geom_area(position = "identity", alpha = .1) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom")

    } else if (x$fm == "Totally fuzzy and Relative") {
      FL.curve = fm_FL(x$results$predicate, x$results$weight)
      x.plot.data = data.frame(x$results, "WECDF" = FL.curve$WECDF)
      x.plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = predicate, y = WECDF)) +
        ggplot2::geom_line() +
        ggplot2::geom_area(position = "identity", alpha = .1) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom")
    } else if (x$fm == "Belhadj (2015)") {
      par.df = data.frame(Parameters = names(x$parameters[-4]),
                          value = unlist(x$parameters[-4]),
                          y = c(0))
      par.lab = bquote(.(x$parameters[4]))
      ggplot2::ggplot(x$results, aes(x = predicate, y = mu)) +
        ggplot2::geom_line(linewidth = .8) +
        ggplot2::geom_area(alpha = .1) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::geom_vline(aes(xintercept = value, colour = Parameters), data = par.df, linetype = "dashed") +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
              legend.position = "bottom")

    } else if (x$fm == "chakravarty" | x$fm == "Cerioli and Zani" | x$fm == "belhadj (2011)"){
      par.df = data.frame(Parameters = names(x$parameters),
                          value = unlist(x$parameters),
                          y = c(0))
      ggplot2::ggplot(x$results, aes(x = predicate, y = mu)) +
        ggplot2::geom_line() + geom_area(alpha = .1) +
        ggplot2::scale_x_continuous("Predicate") +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::geom_vline(aes(xintercept = value, colour = Parameters), data = par.df, linetype = "dashed") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
              legend.position = "bottom")

    } else if (x$fm == "ZBM") {
      par.df <- data.frame(Parameters = names(x$parameters),
                          value = unlist(x$parameters),
                          y = c(0))
      par.df <- par.df[par.df$Parameters %in% c("a", "b", "c"),]

      data.frame( predicate = x$results$predicate,
                  mu= FN(x$results$predicate,
                         a=x$parameters$a,
                         b=x$parameters$b,
                         c=x$parameters$c)) %>%
        dplyr::add_row(
          predicate = x$parameters$a,
          mu = 0
        ) %>%
        dplyr::add_row(
          predicate = x$parameters$a,
          mu = 1
        ) %>%
        dplyr::add_row(
          predicate = x$parameters$b,
          mu = 1
        ) %>%
        dplyr::add_row(
          predicate = x$parameters$c,
          mu = 0
        ) %>%
        ggplot2::ggplot( aes(x = predicate, y = mu)) +
        ggplot2::geom_line(linewidth = .8) +
        ggplot2::geom_area(alpha = .1) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::geom_vline(aes(xintercept = value, colour = Parameters), data = par.df, linetype = "dashed") +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
              legend.position = "bottom")
    }

}
