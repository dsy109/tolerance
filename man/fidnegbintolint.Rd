\name{fidnegbintol.int}
\alias{fidnegbintol.int}
\title{
Fiducial-Based Tolerance Intervals for the Function of Two Negative Binomial Proportions
}
\description{
Provides 1-sided or 2-sided tolerance intervals for the function of two negative binomial proportions using fiducial quantities. 
}
\usage{
fidnegbintol.int(x1, x2, n1, n2, m1 = NULL, m2 = NULL, FUN, 
                 alpha = 0.05, P = 0.99, side = 1, K = 1000, 
                 B = 1000) 
}
\arguments{
    \item{x1}{A value of observed "failures" from group 1.}
    \item{x2}{A value of observed "failures" from group 2.}
    \item{n1}{The target number of successes for group 1.}
    \item{n2}{The target number of successes for group 2.}
    \item{m1}{The total number of future trials for group 1. If \code{NULL}, then it is set to \code{n1}.}
    \item{m2}{The total number of future trials for group 2. If \code{NULL}, then it is set to \code{n2}.}
		\item{FUN}{Any reasonable (and meaningful) function taking exactly two arguments that we are interested in constructing a tolerance interval on.}
    \item{alpha}{The level chosen such that 1-alpha is the confidence level.}
    \item{P}{The proportion of the population to be covered by this tolerance interval.}
    \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2}, respectively).}
    \item{K}{The number of fiducial quantities to be generated.  The number of iterations should be at least as large as the default value of 1000. See \code{Details} for the definition of the fiducial quantity for a negative binomial proportion.}
    \item{B}{The number of iterations used for the Monte Carlo algorithm which determines the tolerance limits. The number of iterations should be at least as large as the default value of 1000.}
}
\details{
If \eqn{X} is observed from a \eqn{NegBin(n,p)} distribution, then the fiducial quantity for \eqn{p} is \eqn{Beta(n,X+0.5)}.
}
\references{
Cai, Y. and Krishnamoorthy, K. (2005), A Simple Improved Inferential Method for Some Discrete Distributions, \emph{Computational Statistics and Data Analysis}, \bold{48}, 605--621.

Clopper, C. J. and Pearson, E. S. (1934), The Use of Confidence or Fiducial Limits Illustrated in the Case of the Binomial, \emph{Biometrika}, \bold{26}, 404--413.

Krishnamoorthy, K. and Lee, M. (2010), Inference for Functions of Parameters in Discrete Distributions Based on Fiducial Approach: Binomial and Poisson Cases, \emph{Journal of Statistical Planning and Inference}, \bold{140}, 1182--1192.

Mathew, T. and Young, D. S. (2013), Fiducial-Based Tolerance Intervals for Some Discrete Distributions, \emph{Computational Statistics and Data Analysis}, \bold{61}, 38--49.
}
\value{
\code{fidnegbintol.int} returns a list with two items.  The first item (\code{tol.limits}) is a data frame with the following items:
		\item{\code{alpha}}{The specified significance level.} 
		\item{\code{P}}{The proportion of the population covered by this tolerance interval.} 
		\item{\code{fn.est}}{A point estimate of the functional form of interest using the maximum likelihood estimates calculated with the inputted values of \code{x1}, \code{x2}, \code{n1}, and \code{n2}.} 
		\item{\code{1-sided.lower}}{The 1-sided lower tolerance bound. This is given only if \code{side = 1.}} 
		\item{\code{1-sided.upper}}{The 1-sided upper tolerance bound. This is given only if \code{side = 1.}} 
		\item{\code{2-sided.lower}}{The 2-sided lower tolerance bound. This is given only if \code{side = 2.}} 
		\item{\code{2-sided.upper}}{The 2-sided upper tolerance bound. This is given only if \code{side = 2.}}
The second item (\code{fn}) simply returns the functional form specified by \code{FUN}. 				
}
\seealso{
\code{\link{fidbintol.int}}, \code{\link{fidpoistol.int}}
}
\examples{
## 95\%/99\% 1-sided and 2-sided tolerance intervals for 
## the ratio of odds ratios for negative binomial proportions.

set.seed(100)

p1 <- 0.6
p2 <- 0.2
n1 <- n2 <- 50
x1 <- rnbinom(1, n1, p1)
x2 <- rnbinom(1, n2, p2)
fun.ti <- function(x, y) x * (1 - y) / (y * (1 - x))

fidnegbintol.int(x1, x2, n1, n2, m1 = 50, m2 = 50, FUN = fun.ti, 
                 alpha = 0.05, P = 0.99, side = 1)
fidnegbintol.int(x1, x2, n1, n2, m1 = 50, m2 = 50, FUN = fun.ti, 
                 alpha = 0.05, P = 0.99, side = 2)
              
}
\keyword{file}