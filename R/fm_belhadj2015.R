#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z1 Parameter
#' @param z2 Parameter
#' @param z Parameter
#' @param b Parameter
#'
#' @return The fuzzy membership function as of Belhadj (2015).
#'
#' @examples
belhadj2015 <- function(x, z1, z2, z, b){
  y <- x
  y[x<z1] <- 1
  y[x>=z2] <- 0
  y[z1<=x & x<z] <- ub1(x[z1<=x & x<z], z1, b)
  y[z<=x & x<z2] <- ub2(x[z<=x & x<z2], z2, b)
  return(y)
}

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z1 Parameter
#' @param b Parameter
#'
#' @return
#' @export
#'
#' @examples
ub1 <- function(x, z1, b){
  return( 1-0.5*((x-z1)/z1)^b)
}

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z2 Parameter
#' @param b Parameter
#'
#' @return
#'
#' @examples
#'
ub2 <- function(x, z2, b){
  return( 0.5*((z2-x)/z2)^b )
}

# ddx_ub2 <- function(x, z2, b){
#   return((1/(2*z2))*b*(b-1)*(1-x/z2)^(b-2))
# }

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z1 Parameter
#' @param z2 Parameter
#' @param b Parameter
#'
#' @export
#'
#' @examples
#'
z_fun <- function(x, z1, z2, b){
  return( ub1(x, z1, b) - ub2(x, z2, b))
}

#' Fuzzy monetary poverty estimation
#'
#' @param x A numeric vector of a monetary variable (or poverty predicate)
#' @param z1 Parameter
#' @param z2 Parameter
#' @param b Parameter
#' @param breakdown A factor of sub-domains to calculate estimates for (using the same alpha).
#' @param weight A numeric vector of sampling weights
#'
#' @return a list containing the fuzzy membership function and the value of z found as of Belhadj(2015).
#'
#' @examples
#' x = sort(rchisq(1000, 15))
#' z1 = 10; z2 = 30
#' b = 4
#' breakdown = sample(letters, 1000, replace = T)
#' weight = rep(1, 1000)
#' fm_belhadj2015(x, z1, z2, b, breakdown, weight)
#' fm_construct(monetary = x, weight, fm = "belhadj", z1 = z1, z2 = z2, b = b)
#' fm_construct(monetary = x, weight, breakdown = breakdown, fm = "belhadj", z1 = z1, z2 = z2, b = b)
#'
fm_belhadj2015 <- function(x, z1, z2, b, breakdown, weight){
  # uniroot(ddx_ub2, z2, b, interval = c(0, 100))
  z <- uniroot(z_fun, z1, z2, b, interval = c(z1, z2), extendInt = "yes")$root
  u <- belhadj2015(x, z1, z2, z, b)
  if(!is.null(breakdown)) {
    u <- tapply( (u*weight)/sum(weight), INDEX = breakdown, mean)
  }
  return(list(estimate = u, z_star = z))
}






