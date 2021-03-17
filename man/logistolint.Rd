\name{logistol.int}
\title{Logistic (or Log-Logistic) Tolerance Intervals}
\alias{logistol.int}
\usage{
logistol.int(x, alpha = 0.05, P = 0.99, log.log = FALSE,
             side = 1)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for data distributed according to a logistic or log-logistic distribution.
}

\arguments{
  \item{x}{A vector of data which is distributed according to a logistic or log-logistic distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{log.log}{If \code{TRUE}, then the data is considered to be from a log-logistic distribution, in which
  case the output gives tolerance intervals for the log-logistic distribution.  The default is \code{FALSE}.} 
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
} \value{
  \code{logistol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} 

\seealso{
  \code{\link{Logistic}}
}
\details{
Recall that if the random variable \eqn{X} is distributed according to a log-logistic distribution, then the random variable \eqn{Y = ln(X)} is
distributed according to a logistic distribution.
}

\references{
  Balakrishnan, N. (1992), \emph{Handbook of the Logistic Distribution}, Marcel Dekker, Inc.

  Hall, I. J. (1975), One-Sided Tolerance Limits for a Logistic Distribution Based on Censored Samples, \emph{Biometrics},
  \bold{31}, 873--880.
} 

\examples{ 
## 90\%/95\% 1-sided logistic tolerance intervals for a sample
## of size 20. 

set.seed(100)
x <- rlogis(20, 5, 1)
out <- logistol.int(x = x, alpha = 0.10, P = 0.95, 
                    log.log = FALSE, side = 1) 
out

plottol(out, x, plot.type = "control", side = "two", 
        x.lab = "Logistic Data")
}

\keyword{file}





