% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fm_construct.R
\name{fm_construct}
\alias{fm_construct}
\title{Fuzzy monetary poverty estimation}
\usage{
fm_construct(
  predicate,
  weight = NULL,
  fm = "verma",
  ID = NULL,
  HCR,
  interval = c(1, 10),
  alpha = NULL,
  hh.size,
  z_min,
  z_max,
  z1,
  z2,
  b,
  z,
  breakdown = NULL,
  data = NULL,
  verbose = FALSE
)
}
\arguments{
\item{predicate}{A numeric vector representing the poverty predicate (i.e. income or expenditure)}

\item{weight}{A numeric vector of sampling weights of the same length of predicate. if NULL weights will set equal to n (n = sample size)}

\item{fm}{The membership function (default is "verma". Other options are "ZBM", "belhadj2015", "belhadj2011", "chakravarty", "cerioli", "verma1999" and "TFR". See Betti et. al., 2023)}

\item{ID}{A numeric or character vector of IDs. if NULL (the default) it is set as the row sequence}

\item{HCR}{If fm="verma" or fm="verma1999" or fm="TFR" . The value of the head count ratio used to compute alpha so that the membership function equals the HCR}

\item{interval}{If fm="verma" or fm="verma1999" or fm="TFR". A numeric vector of length two to look for the value of alpha (if not supplied)}

\item{alpha}{The value of the exponent in equations of "verma", "verma1999" and "TFR". If NULL it is calculated so that it equates the expectation of the membership function to HCR.}

\item{hh.size}{If fm="ZBM". A numeric vector of household size}

\item{z_min}{A parameter of the membership function if fm="belhadj2011", i.e. the z_min: $mu=1 for 0 <y_i<z_min$ (see: See Betti et al.,  2023)}

\item{z_max}{A parameter of the membership function if fm="belhadj2011", i.e. the z_max: $mu=0 for y_i>z_max$ (see: See Betti et al.,  2023)}

\item{z1}{A parameter of the membership function if fm="belhadj2015" or fm="cerioli". For  "belhadj2015" z1: $mu=1 for y_i<z1$ while for  "cerioli" $mu=1 for 0 <y_i<z1$ (see: See Betti et al.,  2023)}

\item{z2}{A parameter of the membership function if fm="belhadj2015" or fm="cerioli". For  "belhadj2015" z2: $mu=0 for y_i>z2$   while for  "cerioli" the z1: $mu=0 for y_i>z2$ (see: See Betti et al.,  2023)}

\item{b}{A parameter of the membership function if fm="belhadj2015". The shape parameter (if b=1 the mf is linear between z1 and z2)}

\item{z}{A parameter of the membership function if fm="chakravarty", i.e. $mu=0 for y_i>=z$ (see: See Betti et al.,  2023)}

\item{breakdown}{A factor of sub-domains to calculate estimates for (using the same alpha)}

\item{data}{An optional data frame containing the variables to be used}

\item{verbose}{Logical. whether to print the proceeding of the procedure}
}
\value{
an object of class FuzzyMonetary containing the (fuzzy) membership function for each individual in the sample,
the estimated expected value (\code{estimate}) of the function and the parameters of the
membership functions (supplied or calculated). If breakdown is supplied it gives an output for each level.
}
\description{
\code{fm_construct} constructs fuzzy monetary poverty estimates.
}
\details{
It implements the fuzzy set approach to monetary poverty measurement where
the usual dichotomy poor (1) not-poor(0) is replaced with a continuum score in $(0,1)$
}
\examples{
#The following examples are based on the dataset eusilc
#included in the package.


#fm = "verma"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             fm = "verma", HCR = 0.154, ID = eusilc$ID)

#fm = "verma1999"
#In this example we set alpha=4.5

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             fm = "verma1999", alpha = 4.5, ID = eusilc$ID)

#fm = "TFR"
#In this example we do not use the sample weights. alpha = 4.5

fm_construct(predicate = eusilc$eq_income,
             fm = "TFR", alpha = 4.5)

#fm = "belhadj2015"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             z1=100, z2=15000, b=2,
             fm = "belhadj2015")

#fm = "cerioli"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             z1=100, z2=10000, fm= "cerioli")

#fm = "belhadj2011"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             z_min=1000, z_max=8000, fm= "belhadj2011")

#fm = "chakravarty"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             z=8000, fm= "chakravarty")

#fm = "ZBM"
#For this index have to use the household size

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             hh.size=eusilc$ncomp , fm= "ZBM")

#######################
##Including breakdown##
#######################

#fm = "verma"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             fm = "verma", HCR = 0.154, ID = eusilc$ID,
             breakdown = eusilc$db040)

#fm = "verma1999"
#In this example we set alpha=4.5

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             fm = "verma1999", alpha = 4.5, ID = eusilc$ID,
             breakdown = eusilc$db040)

#fm = "TFR"
#In this example we do not use the sample weights. alpha = 4.5

fm_construct(predicate = eusilc$eq_income,
             fm = "TFR", alpha = 4.5,
             breakdown = eusilc$db040)

#fm = "belhadj2015"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             z1=100, z2=15000, b=2,
             fm = "belhadj2015", breakdown = eusilc$db040)

#fm = "cerioli"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             z1=100, z2=10000, fm= "cerioli", breakdown = eusilc$db040)

#fm = "belhadj2011"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             z_min=1000, z_max=8000, fm= "belhadj2011",
             breakdown = eusilc$db040)

#fm = "chakravarty"

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             z=8000, fm= "chakravarty", breakdown = eusilc$db040)

#fm = "ZBM"
#For this index we have to use the household size

fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
             hh.size=eusilc$ncomp , fm= "ZBM",
             breakdown = eusilc$db040)
}
\references{
Belhadj, B. (2011). A new fuzzy unidimensional poverty index from an information theory perspective. Empirical Economics, 40(1):687–704.

Belhadj, B. (2015). Employment measure in developing countries via minimum wage and poverty new fuzzy approach. Opsearch, 52(1):329–339.

Betti, G., Cheli, B., Lemmi, A., and Verma, V. (2006). Multidimensional and longitudinal poverty: an integrated fuzzy approach. In Betti, G. and Lemmi, A., editors, Fuzzy set approach to multidimensional poverty measurement, pages 115–137. Springer, Boston, USA.

Betti, G., D’Agostino, A., Lemmi, A., & Neri, L. (2023). The fuzzy approach to poverty measurement. In Research Handbook on Measuring Poverty and Deprivation Edited by Silber, J. (pp. 489-500). Edward Elgar Publishing.

Betti, G. and Verma, V. (1999). Measuring the degree of poverty in a dynamic and comparative context: a multi-dimensional approach using fuzzy set theory. In Proceedings, iccs-vi, volume 11, pages 289–300.

Cerioli, A. and Zani, S. (1990). A fuzzy approach to the measurement of poverty. In Income and Wealth Distribution, Inequality and Poverty: Proceedings of the Second International Conference on Income Distribution by Size: Generation, Distribution, Measurement and Applications., 272–284. Springer, Boston, USA.

Chakravarty, S. R. (2006). An Axiomatic Approach to Multidimensional Poverty Measurement via Fuzzy Sets. Fuzzy Set Approach to Multidimensional Poverty Measurement, 49-72.

Cheli, B. and Lemmi, A. (1995). A ’totally’ fuzzy and relative approach to the multidimensional analysis of poverty. 24(1):115–134.

Zedini, A. and Belhadj, B. (2015). A new approach to unidimensional poverty analysis: Application to the Tunisian case. Review of Income and Wealth, 61(3):465–476.
}
