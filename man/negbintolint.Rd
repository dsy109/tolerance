\name{negbintol.int}
\title{Negative Binomial Tolerance Intervals}
\alias{negbintol.int}
\usage{
negbintol.int(x, n, m = NULL, alpha = 0.05, P = 0.99, 
              side = 1, method = c("LS", "WU", "CB", 
              "CS", "SC", "LR", "SP", "CC"))
}
\description{
Provides 1-sided or 2-sided tolerance intervals for negative binomial random variables. From a statistical quality control perspective, these limits use the number of failures that occur to reach \code{n} successes to bound the number of failures for a specified amount of future successes (\code{m}). 
}

\arguments{
  \item{x}{The total number of failures that occur from a sample of size \code{n}.  Can be a vector of length \code{n}, in which case the sum of \code{x} is computed.}
  \item{n}{The target number of successes (sometimes called size) for each trial.}
  \item{m}{The target number of successes in a future lot for which the tolerance limits will be calculated. If \code{m = NULL}, then the tolerance limits will be constructed assuming \code{n} for the target number of future successes.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the defective (or acceptable) units in future samples of size \code{m} 
  to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for calculating the lower and upper confidence bounds, which are used in the calculation
  of the tolerance bounds.  The default method is \code{"LS"}, which is the large-sample method based on the MLE.  \code{"WU"} is a Wald-type interval based on the UMVUE of the negative binomial proportion. \code{"CB"} is the Casella-Berger exact method.  \code{"CS"} is a method based on chi-square percentiles.  \code{"SC"} is the score method.  \code{"LR"} is a likelihood ratio-based method.  \code{"SP"} is a method using a saddlepoint approximation for the confidence intervals.  \code{"CC"} gives a continuity-corrected version of the large-sample method and is appropriate when \code{n} is large.  More information on these methods can be found in the "References".}
} \value{
  \code{negbintol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of defective (or acceptable) units in future samples of size \code{m}.}
  \item{pi.hat}{The probability of success in each trial, calculated by \code{n/(n+x)}.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{NegBinomial}}, \code{\link{umatol.int}}
}

\references{
  Casella, G. and Berger, R. L. (1990), \emph{Statistical Inference}, Duxbury Press.

  Hahn, G. J. and Chandra, R. (1981), Tolerance Intervals for Poisson and Binomial Variables,
  \emph{Journal of Quality Technology}, \bold{13}, 100--110.

  Tian, M., Tang, M. L., Ng, H. K. T., and Chan, P. S. (2009), A Comparative Study of Confidence Intervals for Negative Binomial Proportions,
  \emph{Journal of Statistical Computation and Simulation}, \bold{79}, 241--249.

  Young, D. S. (2014), A Procedure for Approximate Negative Binomial Tolerance Intervals, \emph{Journal of Statistical Computation and Simulation}, \bold{84}, 438--450.
} 
\note{
Recall that the geometric distribution is the negative binomial distribution where the size is 1. Therefore, the case when \code{n = m = 1} will provide tolerance limits for a geometric distribution.
}
\details{
This function takes the approach for Poisson and binomial random variables developed in Hahn and Chandra (1981) and applies it to the negative binomial case.
}
\examples{ 
## Comparison of 95\%/99\% 1-sided tolerance limits with
## 50 failures before 10 successes are reached.

negbintol.int(x = 50, n = 10, side = 1, method = "LS")
negbintol.int(x = 50, n = 10, side = 1, method = "WU")
negbintol.int(x = 50, n = 10, side = 1, method = "CB")
negbintol.int(x = 50, n = 10, side = 1, method = "CS")
negbintol.int(x = 50, n = 10, side = 1, method = "SC")
negbintol.int(x = 50, n = 10, side = 1, method = "LR")
negbintol.int(x = 50, n = 10, side = 1, method = "SP")
negbintol.int(x = 50, n = 10, side = 1, method = "CC")

## 95\%/99\% 1-sided tolerance limits and 2-sided tolerance 
## interval for the same setting above, but when we are 
## interested in a future experiment that requires 20 successes 
## be reached for each trial.

negbintol.int(x = 50, n = 10, m = 20, side = 1)
negbintol.int(x = 50, n = 10, m = 20, side = 2)
}

\keyword{file}

