% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fm_mu.R
\name{fm_mu}
\alias{fm_mu}
\title{Fuzzy monetary poverty estimation}
\usage{
fm_mu(monetary.ord, weight.ord, alpha)
}
\arguments{
\item{monetary.ord}{A sorted vector of a monetary variable (in ascending order).}

\item{weight.ord}{A sorted vector of weights (in the same order of s.ord)}

\item{alpha}{The value of the exponent parameter to use in the non-linear equation as of Betti et. al, 2018.}
}
\value{
A numeric vector containing the estimated membership function.
}
\description{
This function calculates the fuzzy membership function as defined in Betti et. al, 2018.
}
