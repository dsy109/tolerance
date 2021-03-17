\name{exp2tol.int}
\title{2-Parameter Exponential Tolerance Intervals}
\alias{exp2tol.int}
\usage{
exp2tol.int(x, alpha = 0.05, P = 0.99, side = 1,
            method = c("GPU", "DUN", "KM"), type.2 = FALSE)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for data distributed according to a 2-parameter
  exponential distribution.  Data with Type II censoring is permitted.
}

\arguments{
  \item{x}{A vector of data which is distributed according to the 2-parameter exponential distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for how the upper tolerance bound is approximated.  \code{"GPU"} is the
  Guenther-Patil-Upppuluri method. \code{"DUN"} is the Dunsmore method, which has been empirically shown to be an improvement
  for samples greater than or equal to 8.  \code{"KM"} is the Krishnamoorthy-Mathew method, which is typically more liberal than the other methods. More information on these methods can be found in the "References", which also highlight general sample size conditions as to when these different methods should be used.}
  \item{type.2}{Select \code{TRUE} if Type II censoring is present (i.e., the data set is censored at the maximum
  value present).  The default is \code{FALSE}.} 
} \value{
  \code{exp2tol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{TwoParExponential}}
}

\references{
  Dunsmore, I. R. (1978), Some Approximations for Tolerance Factors for the Two Parameter Exponential Distribution,
  \emph{Technometrics}, \bold{20}, 317--318.

  Engelhardt, M. and Bain, L. J. (1978), Tolerance Limits and Confidence Limits on Reliability for the Two-Parameter
  Exponential Distribution, \emph{Technometrics}, \bold{20}, 37--39.
  
  Guenther, W. C., Patil, S. A., and Uppuluri, V. R. R. (1976), One-Sided \eqn{\beta}-Content Tolerance Factors
  for the Two Parameter Exponential Distribution, \emph{Technometrics}, \bold{18}, 333--340.  

  Krishnamoorthy, K. and Mathew, T. (2009), \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}, Wiley.
} 

\examples{ 
## 95\%/90\% 1-sided 2-parameter exponential tolerance intervals
## for a sample of size 50. 

set.seed(100)
x <- r2exp(50, 6, shift = 55)
out <- exp2tol.int(x = x, alpha = 0.05, P = 0.90, side = 1,
                   method = "DUN", type.2 = FALSE)
out

plottol(out, x, plot.type = "both", side = "upper", 
        x.lab = "2-Parameter Exponential Data")
}

\keyword{file}


