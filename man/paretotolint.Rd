\name{paretotol.int}
\title{Pareto (or Power Distribution) Tolerance Intervals}
\alias{paretotol.int}
\usage{
paretotol.int(x, alpha = 0.05, P = 0.99, side = 1,
              method = c("GPU", "DUN"), power.dist = FALSE)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for data distributed according to either a Pareto distribution or 
  a power distribution (i.e., the inverse Pareto distribution).
}

\arguments{
  \item{x}{A vector of data which is distributed according to either a Pareto distribution or a power distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for how the upper tolerance bound is approximated when transforming to utilize the relationship with the 2-parameter
  exponential distribution.  \code{"GPU"} is the Guenther-Patil-Upppuluri method. \code{"DUN"} is the Dunsmore method, which was empirically shown to be an improvement
  for samples greater than or equal to 8.  More information on these methods can be found in the "References".}
  \item{power.dist}{If \code{TRUE}, then the data is considered to be from a power distribution, in which
  case the output gives tolerance intervals for the power distribution.  The default is \code{FALSE}.} 
} \value{
  \code{paretotol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{TwoParExponential}}, \code{\link{exp2tol.int}}
} 

\details{ Recall that if the random variable \eqn{X} is distributed 
according to a Pareto distribution, then the random variable \eqn{Y 
= ln(X)} is distributed according to a 2-parameter exponential 
distribution. Moreover, if the random variable \eqn{W} is 
distributed according to a power distribution, then the random 
variable \eqn{X = 1/W} is distributed according to a Pareto 
distribution, which in turn means that the random variable \eqn{Y = 
ln(1/W)} is distributed according to a 2-parameter exponential 
distribution.}

\references{
  Dunsmore, I. R. (1978), Some Approximations for Tolerance Factors for the Two Parameter Exponential Distribution,
  \emph{Technometrics}, \bold{20}, 317--318.

  Engelhardt, M. and Bain, L. J. (1978), Tolerance Limits and Confidence Limits on Reliability for the Two-Parameter
  Exponential Distribution, \emph{Technometrics}, \bold{20}, 37--39.
  
  Guenther, W. C., Patil, S. A., and Uppuluri, V. R. R. (1976), One-Sided \eqn{\beta}-Content Tolerance Factors
  for the Two Parameter Exponential Distribution, \emph{Technometrics}, \bold{18}, 333--340.  

  Krishnamoorthy, K., Mathew, T., and Mukherjee, S. (2008), Normal-Based Methods for a Gamma Distribution:
  Prediction and Tolerance Intervals and Stress-Strength Reliability, \emph{Technometrics}, \bold{50}, 69--78.
} 

\examples{ 
## 95\%/99\% 2-sided Pareto tolerance intervals 
## for a sample of size 500. 

set.seed(100)
x <- exp(r2exp(500, rate = 0.15, shift = 2))
out <- paretotol.int(x = x, alpha = 0.05, P = 0.99, side = 2,
                     method = "DUN", power.dist = FALSE)
out

plottol(out, x, plot.type = "both", side = "two", 
        x.lab = "Pareto Data")
}

\keyword{file}


