\name{hypertol.int}
\title{Hypergeometric Tolerance Intervals}
\alias{hypertol.int}
\usage{
hypertol.int(x, n, N, m = NULL, alpha = 0.05, P = 0.99, 
             side = 1, method = c("EX", "LS", "CC"))
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for hypergeometric random variables.  From a sampling without replacement
  perspective, these limits use the proportion of units from group A (e.g., "black balls" in an urn) in a sample to bound the number
  of potential units drawn from group A in a future sample taken from the universe.
}

\arguments{
  \item{x}{The number of units from group A in the sample. Can be a vector, in which case the sum of \code{x} is used.}
  \item{n}{The size of the random sample of units selected.}
  \item{N}{The population size.}
  \item{m}{The quantity of units to be sampled from the universe for a future study. If \code{m = NULL}, then the tolerance limits will be constructed assuming \code{n} for this quantity.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of units from group A in future samples of size \code{m} 
  to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for calculating the lower and upper confidence bounds, which are used in the calculation
  of the tolerance bounds.  The default method is \code{"EX"}, which is an exact-based method.  \code{"LS"} is the large-sample method.  \code{"CC"} gives a continuity-corrected version of the large-sample method.}
} \value{
  \code{hypertol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of units from group A in future samples of size \code{m}.}
  \item{rate}{The sampling rate determined by \code{n/N}.}
  \item{p.hat}{The proportion of units in the sample from group A, calculated by \code{x/n}.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{acc.samp}}, \code{\link{Hypergeometric}}
}
\note{
As this methodology is built using large-sample theory, if the sampling rate is less than 0.05, then a warning is generated stating that the results are not reliable.  Also, compare the functionality of this procedure with the \code{\link{acc.samp}} procedure, which is to determine a minimal acceptance limit for a particular sampling plan.
}
\references{
  Brown, L. D., Cai, T. T., and DasGupta, A. (2001), Interval Estimation for a Binomial Proportion, 
  \emph{Statistical Science}, \bold{16}, 101--133.

  Eichenberger, P., Hulliger, B., and Potterat, J. (2011), Two Measures for Sample Size Determination,
  \emph{Survey Research Methods}, \bold{5}, 27--37.

  Young, D. S. (2014), Tolerance Intervals for Hypergeometric and Negative Hypergeometric Variables,
  \emph{Sankhya: The Indian Journal of Statistics, Series B}, \bold{77}(1), 114--140.
} 

\examples{ 
## 90\%/95\% 1-sided and 2-sided hypergeometric tolerance 
## intervals for a future sample of 30 when the universe
## is of size 100.

hypertol.int(x = 15, n = 50, N = 100, m = 30, alpha = 0.10, 
             P = 0.95, side = 1, method = "LS")
hypertol.int(x = 15, n = 50, N = 100, m = 30, alpha = 0.10, 
             P = 0.95, side = 1, method = "CC")
hypertol.int(x = 15, n = 50, N = 100, m = 30, alpha = 0.10, 
             P = 0.95, side = 2, method = "LS")
hypertol.int(x = 15, n = 50, N = 100, m = 30, alpha = 0.10, 
             P = 0.95, side = 2, method = "CC")
 }

\keyword{file}


