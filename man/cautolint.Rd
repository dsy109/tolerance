\name{cautol.int}
\title{Cauchy Tolerance Intervals}
\alias{cautol.int}
\usage{
cautol.int(x, alpha = 0.05, P = 0.99, side = 1)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for Cauchy distributed data.
}

\arguments{
  \item{x}{A vector of data which is Cauchy distributed.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}

} \value{
  \code{cautol.int} returns a data.frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{Cauchy}}
}

\references{
  Bain, L. J. (1978), \emph{Statistical Analysis of Reliability and Life-Testing Models}, Marcel Dekker, Inc.
} 

\examples{ 
## 95\%/90\% 2-sided Cauchy tolerance interval for a sample 
## of size 1000. 

set.seed(100)
x <- rcauchy(1000, 100000, 10)
out <- cautol.int(x = x, alpha = 0.05, P = 0.90, side = 2)
out

plottol(out, x, plot.type = "both", x.lab = "Cauchy Data")
}

\keyword{file}

