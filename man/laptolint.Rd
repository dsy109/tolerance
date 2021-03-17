\name{laptol.int}
\title{Laplace Tolerance Intervals}
\alias{laptol.int}
\usage{
laptol.int(x, alpha = 0.05, P = 0.99, side = 1)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for data distributed according to a Laplace distribution.
}

\arguments{
  \item{x}{A vector of data which is distributed according to a Laplace distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
} \value{
  \code{laptol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} 

\references{
  Bain, L. J. and Engelhardt, M. (1973), Interval Estimation for the Two Parameter Double Exponential Distribution, 
  \emph{Technometrics}, \bold{15}, 875--887.
} 

\examples{ 
## First generate data from a Laplace distribution with location
## parameter 70 and scale parameter 3.

set.seed(100)
tmp <- runif(40)
x <- rep(70, 40) - sign(tmp - 0.5)*rep(3, 40)*
              log(2*ifelse(tmp < 0.5, tmp, 1-tmp))

## 95\%/90\% 1-sided Laplace tolerance intervals for the sample
## of size 40 generated above. 

out <- laptol.int(x = x, alpha = 0.05, P = 0.90, side = 1) 
out

plottol(out, x, plot.type = "hist", side = "two", 
        x.lab = "Laplace Data")
}

\keyword{file}




