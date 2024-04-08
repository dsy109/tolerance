\name{semiconttol.int}
\title{Generalized Intervals for Semicontinuous Data}
\alias{semiconttol.int}
\usage{
semiconttol.int(x, alpha = 0.05, P = 0.99, N = 1000)
}
\description{
  Provides confidence intervals, one-sided prediction limits, and one-sided tolerance limits for semicontinuous data --- either zero-inflated gamma (ZIG) or zero-inflated lognormal (ZILN) distribution --- using a generalized fiducial framework.
}

\arguments{
  \item{x}{A vector of semicontinuous data.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{N}{The number of fiducial samples to generate.}
} \value{
  \code{semiconttol.int} returns a list with items:
  \item{ZIG.CI}{The generalized confidence interval under a ZIG distribution.}
  \item{ZIG.PI}{The generalized (upper) prediction limit under a ZIG distribution.}
  \item{ZIG.TI}{The generalized (upper) tolerance limit under a ZIG distribution.}
  \item{ZIG.TI.appx}{The generalized (upper) tolerance limit under a ZIG distribution based on the Wilson-Hilferty approximation.}
  \item{ZILN.CI}{The generalized confidence interval under a ZILN distribution.}
  \item{ZILN.PI}{The generalized (upper) prediction limit under a ZILN distribution.}
  \item{ZILN.TI}{The generalized (upper) tolerance limit under a ZILN distribution.}
  \item{ZILN.TI.appx}{The generalized (upper) tolerance limit under a ZILN distribution based on an approximation used in Hasan and Krishnamoorthy (2018).}
  \item{`NA`}{The number of times generalized fiducial quantities could not be calculated due to unlucky samples being drawn; e.g., a sample with all 0s.  This will happen rarely and usually only when there is a very large proportion of zeros.}
} \seealso{
  \code{\link{fidbintol.int}}, \code{\link{fidnegbintol.int}}, \code{\link{fidpoistol.int}}
}

\references{
  Hasan, M. S. and Krishnamoorthy, K. (2018), Confidence Intervals for the Mean and a Percentile Based on Zero-Inflated Lognormal Data, \emph{Journal of Statistical Computation and Simulation}, \bold{88}, 1499--1514.

  Zou, Y. and Young, D. S. (2024), Fiducial-Based Statistical Intervals for Zero-Inflated Gamma Data, \emph{Journal of Statistical Theory and Practice}, \bold{18}, 1--20.
} 

\examples{ 
## Generalized intervals assuming 95\% confidence and
## 95\% content for a dataset analyzed in Hasan and
## Krishnamoorthy (2018).

x <- c(6, 0, 6, 9, 6.5, 0, 0, 0, 1, 0.5, 2, 2, 0, 0, 1)
set.seed(1)
out <- semiconttol.int(x, P = 0.95, alpha = 0.05, N = 500)
out
}

\keyword{file}




