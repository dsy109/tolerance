\name{simnormtol.int}
\title{Simultaneous Normal (or Log-Normal) Tolerance Intervals}
\alias{simnormtol.int}
\usage{
simnormtol.int(x, alpha = 0.05, P = 0.99, side = 1,
               method = c("EXACT", "BONF"), m = 50, log.norm = FALSE)
}
\description{
  Provides simultaneous 1-sided or 2-sided tolerance intervals for data distributed according to either a normal
  distribution or log-normal distribution.
}

\arguments{
  \item{x}{Either a matrix or list of vectors of the data.  If a matrix, then the columns are the samples from the different normal (or log-normal) populations.  If \code{method = "EXACT"}, then \code{x} must be a matrix.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{side}{Whether simultaneous 1-sided or 2-sided tolerance intervals are required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for calculating the k-factors.  \code{"EXACT"} is an exact method that can be used when all \code{l} groups have the same sample size. \code{"BONF"} is an approximate method using the Bonferroni inequality, which can be used when the \code{l} groups have different sample sizes.}
  \item{m}{The maximum number of subintervals to be used in the \code{integrate} function.  This is necessary only for \code{method = "EXACT"}.  The larger
  the number, the more accurate the solution.  Too low of a value can result in an error. A large value can also cause the function to be slow for \code{method = "EXACT"}.}
  \item{log.norm}{If \code{TRUE}, then the data is considered to be from a log-normal distribution, in which
  case the output gives tolerance intervals for the log-normal distribution.  The default is \code{FALSE}.} 
} \value{
  \code{normtol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{x.bar}{The sample means.}
  \item{1-sided.lower}{The simultaneous 1-sided lower tolerance bounds.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The simultaneous 1-sided upper tolerance bounds.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The simultaneous 2-sided lower tolerance bounds.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The simultaneous 2-sided upper tolerance bounds.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{Normal}}, \code{\link{K.factor.sim}}
}

\details{
Recall that if the random variable \eqn{X} is distributed according to a log-normal distribution, then the random variable \eqn{Y = ln(X)} is
distributed according to a normal distribution.
}

\note{The code for this functions is built upon code provided by Andrew Landgraf.}

\references{
  Krishnamoorthy, K. and Mathew, T. (2009), \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}, Wiley.

  Mee, R. W. (1990), Simultaneous Tolerance Intervals for Normal Populations with Common Variance, \emph{Technometrics}, \bold{32}, 83-92.
} 

\examples{ 
## 95\%/95\% simultaneous 1-sided normal tolerance 
## intervals for two samples of unequal size. 

set.seed(100)
x <- list(rnorm(5,1),rnorm(7,1,2))
out <- simnormtol.int(x = x, alpha = 0.05, P = 0.95, 
                      side = 1, method = "BONF")
out
}

\keyword{file}




