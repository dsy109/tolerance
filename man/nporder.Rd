\name{np.order}
\title{Sample Size Determination for Tolerance Limits Based on Order Statistics}
\alias{np.order}
\usage{
np.order(m, alpha = 0.05, P = 0.99, indices = FALSE)
}
\description{
  For given values of \code{m}, \code{alpha}, and \code{P}, this function solves the necessary sample size such that the
  \code{r}-th (or (\code{n-s+1})-th) order statistic is the \code{[100(1-alpha)\%, 100(P)\%]} lower (or upper) tolerance
  limit (see the Details section below for further explanation).  This function can also report all combinations of order
  statistics for 2-sided intervals.
}

\arguments{
  \item{m}{See the Details section below for how \code{m} is defined.}
  \item{alpha}{1 minus the confidence level attained when it is desired to cover a proportion \code{P}
  of the population with the order statistics.}
  \item{P}{The proportion of the population to be covered with confidence \code{1-alpha} with the order statistics.}
  \item{indices}{An optional argument to report all combinations of order statistics indices for the upper and lower limits
  of the 2-sided intervals.  Note that this can only be calculated when \code{m>1}.}

} \value{
  If \code{indices = FALSE}, then a single number is returned for the necessary sample size such that the
  \code{r}-th (or (\code{n-s+1})-th) order statistic is the \code{[100(1-alpha)\%, 100(P)\%]} lower (or upper) tolerance
  limit.  If \code{indices = TRUE}, then a list is returned with a single number for the necessary sample size and a matrix
  with 2 columns where each row gives the pairs of indices for the order statistics for all permissible \code{[100(1-alpha)\%, 100(P)\%]}
  2-sided tolerance intervals. 
} \seealso{
  \code{\link{nptol.int}}
} \details{
  For the 1-sided tolerance limits, \code{m=s+r} such that the probability is at least \code{1-alpha} that at least the 
  proportion \code{P} of the population is below the (\code{n-s+1})-th order statistic for the upper limit or above the \code{r}-th order statistic
  for the lower limit.  This means for the 1-sided upper limit that \code{r=1}, while for the 1-sided lower limit it means that \code{s=1}. 
  For the 2-sided tolerance intervals, \code{m=s+r} such that the probability is at least \code{1-alpha} that at least the 
  proportion \code{P} of the population is between the \code{r}-th and (\code{n-s+1})-th order statistics. Thus, all combinations of r>0 and
  s>0 such that \code{m=s+r} are considered.
}


\references{
  Hanson, D. L. and Owen, D. B. (1963), Distribution-Free Tolerance Limits Elimination of the Requirement That
  Cumulative Distribution Functions Be Continuous, \emph{Technometrics}, \bold{5}, 518--522. 

  Scheffe, H. and Tukey, J. W. (1945), Non-Parametric Estimation I. Validation of Order Statistics, 
  \emph{Annals of Mathematical Statistics}, \bold{16}, 187--192. 
} 

\examples{ 
## Only requesting the sample size.

np.order(m = 5, alpha = 0.05, P = 0.95)

## Requesting the order statistics indices as well.

np.order(m = 5, alpha = 0.05, P = 0.95, indices = TRUE)

}

\keyword{file}


