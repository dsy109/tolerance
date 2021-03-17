\name{uniftol.int}
\title{Uniform Tolerance Intervals}
\alias{uniftol.int}
\usage{
uniftol.int(x, alpha = 0.05, P = 0.99, upper = NULL,
            lower = NULL, side = 1)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for data distributed according to a uniform distribution.
}

\arguments{
  \item{x}{A vector of data which is distributed according to a uniform distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{upper}{The upper bound of the data.  When \code{NULL}, then the maximum of \code{x} is used.}
  \item{lower}{The lower bound of the data.  When \code{NULL}, then the minimum of \code{x} is used.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
} \value{
  \code{uniftol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} 

\references{
  Faulkenberry, G. D. and Weeks, D. L. (1968), Sample Size Determination for Tolerance Limits,
  \emph{Technometrics}, \bold{10}, 343--348.
} 

\examples{ 
## 90\%/90\% 1-sided uniform tolerance intervals for a sample
## of size 50 with a known lower bound of 0. 

set.seed(100)
x <- runif(50, 0, 50)
out <- uniftol.int(x = x, alpha = 0.10, P = 0.90, lower = 0,
                   side = 1) 
out

plottol(out, x, plot.type = "hist", side = "two", 
        x.lab = "Uniform Data")
}

\keyword{file}





