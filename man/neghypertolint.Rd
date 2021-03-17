\name{neghypertol.int}
\title{Negative Hypergeometric Tolerance Intervals}
\alias{neghypertol.int}
\usage{
neghypertol.int(x, n, N, m = NULL, alpha = 0.05, P = 0.99,
                side = 1, method = c("EX", "LS", "CC"))
}

\description{
  Provides 1-sided or 2-sided tolerance intervals for negative hypergeometric random variables.  When sampling without replacement, these limits are on the total number of expected draws in a future sample in order to achieve a certain number from group A (e.g., "black balls" in an urn).
}

\arguments{
  \item{x}{The number of units drawn in order to achieve \code{n} successes. Can be a vector, in which case the sum of \code{x} is used.}
  \item{n}{The target number of successes in the sample drawn (e.g., the number of "black balls" you are to draw in the sample).}
  \item{N}{The population size (e.g., the total number of balls in the urn).}
  \item{m}{The target number of successes to be sampled from the universe for a future study. If \code{m = NULL}, then the tolerance limits will be constructed assuming \code{n} for this quantity.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of units from group A in future samples of size \code{m} 
  to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for calculating the lower and upper confidence bounds, which are used in the calculation
  of the tolerance bounds.  The default method is \code{"EX"}, which is an exact-based method.  \code{"LS"} is the large-sample method.  \code{"CC"} gives a continuity-corrected version of the large-sample method.}
}
 \value{
  \code{neghypertol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of units from group A in future samples of size \code{m}.}
  \item{rate}{The sampling rate determined by \code{x/N}.}
  \item{p.hat}{The proportion of units in the sample from group A, calculated by \code{n/x}.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} 
\note{
As this methodology is built using large-sample theory, if the sampling rate is less than 0.05, then a warning is generated stating that the results are not reliable.
}
\references{
  Khan, R. A. (1994), A Note on the Generating Function of a Negative Hypergeometric Distribution,
  \emph{Sankhya: The Indian Journal of Statistics, Series B}, \bold{56}, 309--313.

  Young, D. S. (2014), Tolerance Intervals for Hypergeometric and Negative Hypergeometric Variables,
  \emph{Sankhya: The Indian Journal of Statistics, Series B}, \bold{77}(1), 114--140.
} 
\seealso{
  \code{\link{acc.samp}}, \code{\link{NegHypergeometric}}
}
\examples{
## 90\%/95\% 2-sided negative hypergeometric tolerance
## intervals for a future number of 20 successes when
## the universe is of size 100.  The estimates are 
## based on having drawn 50 in another sample to achieve 
## 20 successes.

neghypertol.int(50, 20, 100, m = 20, alpha = 0.05, 
                P = 0.95, side = 2, method = "LS")
}
\keyword{file}

