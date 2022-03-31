\name{npmvtol.region}
\title{Nonparametric Multivariate Hyperrectangular Tolerance Regions}
\alias{npmvtol.region}
\usage{
npmvtol.region(x, alpha = NULL, P = NULL, Beta = NULL, depth.fn, 
               adjust = c("no", "floor", "ceiling"), 
               type = c("central", "semispace"), 
               semi.order = list(lower = NULL, center = NULL, upper = NULL), 
               L = -Inf, U = Inf, ...)
}
\description{
  Provides depth-based multivariate central or semi-space nonparametric tolerance regions.  These can be calculated for any continuous multivariate data set.  Either (P, 1-alpha) tolerance regions or beta-expectation tolerance regions can be specified.
}

\arguments{
  \item{x}{An \code{n}x\code{p} matrix of data assumed to be drawn from a \code{p}-dimensional multivariate distribution.  \code{n} pertains to the sample size.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level. Note that if a (P, 1-alpha) tolerance region is required, then both \code{alpha} and \code{P} must be specified, but \code{Beta} must be set to \code{NULL}.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.  Note that if a (P, 1-alpha) tolerance region is required, then both \code{alpha} and \code{P} must be specified, but \code{Beta} must be set to \code{NULL}.}
  \item{Beta}{The confidence level for a beta-expectation tolerance region. Note that if a beta-expectation tolerance region is required, then \code{Beta} must be specified, but both \code{alpha} and \code{P} must be set to \code{NULL}.}
  \item{depth.fn}{The data depth function used to perform the ordering of the multivariate data.  Thus function must be coded in such a way that the first argument is multivariate data for which to calculate the depth values and the second argument is the original multivariate sample, \code{x}.  For the purpose of this tolerance region calculation, these two arguments should both be the original multivariate sample.}
  \item{adjust}{Whether an adjustment should be made during an intermediate calculation for determining the number of points that need to be included in the multivariate region.  If \code{adjust = "no"}, the default, then no adjustment is made during the intermediate calculation.  If \code{adjust = "floor"}, then the intermediate calculation is rounded down to the next nearest integer.  If \code{adjust = "ceiling"}, then the intermediate calculation is rounded up to the next nearest integer.}
  \item{type}{The type of multivariate hyperrectangular region to calculate.  If \code{type = "central"}, then two-sided intervals are reported for each dimension of the data \code{x}.  If \code{type = "semispace"}, then a combination of one-sided intervals and two-sided intervals are reported for the dimensions of \code{x}.  Which interval is calculated for each dimension in this latter setting is dictated by the \code{semi.order} argument.}
  \item{semi.order}{If \code{type = "semispace"}, then this argument must be specified.  This argument is a list of length 3, such that each element gives the indices of the dimensions of \code{x} for which the type of interval should be calculated.  Indices specified for the element of \code{lower} will return one-sided lower limits for those dimensions, indices specified for the element of \code{center} will return two-sided intervals for those dimensions, and indices specified for the element of \code{upper} will return one-sided upper limits for those dimensions.}
  \item{L}{If \code{type = "semispace"}, these are the lower limits for any dimensions for which one requests one-sided upper limits.}
  \item{U}{If \code{type = "semispace"}, these are the upper limits for any dimensions for which one requests one-sided lower limits.}
    \item{...}{Additional arguments passed to the \code{depth.fn} function.}
} \value{
  \code{npmvtol.region} returns a \code{p}x\code{2} matrix where the columns give the lower and upper limits, respectively, of the multivariate hyperrectangular tolerance region.
} \seealso{
  \code{\link{distfree.est}}, \code{\link{mvtol.region}}, \code{\link{npregtol.int}}
}

\references{
  Young, D. S. and Mathew, T. (2020), Nonparametric Hyperrectangular Tolerance
and Prediction Regions for Setting Multivariate Reference Regions in Laboratory
Medicine, \emph{Statistical Methods in Medical Research}, \bold{29}, 3569--3585.
} 

\examples{ 
## 90\%/95\% semi-space tolerance region for a sample
## of size 20 generated from a multivariate normal
## distribution. The mdepth function below is not 
## a true depth function, but used only for
## illustrative purposes.

mdepth <- function(pts, x){
          mahalanobis(pts, center = rep(0, 3),
                      cov = diag(1, 3))
          }

set.seed(100)
x <- cbind(rnorm(100), rnorm(100), rnorm(100))
out <- npmvtol.region(x = x, alpha = 0.10, P = 0.95, depth.fn = mdepth,
                      type = "semispace", semi.order = list(lower = 2, 
                      center = 3, upper = 1))
out
}

\keyword{file}




