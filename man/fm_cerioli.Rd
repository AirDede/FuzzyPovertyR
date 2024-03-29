% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fm_cz.R
\name{fm_cerioli}
\alias{fm_cerioli}
\title{Fuzzy monetary poverty estimation}
\usage{
fm_cerioli(x, z1, z2, weight, breakdown, ID)
}
\arguments{
\item{x}{poverty predicate}

\item{z1}{parameter}

\item{z2}{parameter}

\item{weight}{A numeric vector of sampling weights. if NULL simple random sampling weights will be used.}

\item{breakdown}{A factor of sub-domains to calculate estimates for.}

\item{ID}{A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence.}
}
\value{
a list containing the membership function values and its expected value
}
\description{
constructs fuzzy monetary poverty estimates as of Cerioli Zani
}
