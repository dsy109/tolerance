\name{exptol.int}
\title{Exponential Tolerance Intervals}
\alias{exptol.int}
\usage{
exptol.int(x, alpha = 0.05, P = 0.99, side = 1, type.2 = FALSE)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for data distributed according to an
  exponential distribution.  Data with Type II censoring is permitted.
}

\arguments{
  \item{x}{A vector of data which is distributed according to an exponential distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{type.2}{Select \code{TRUE} if Type II censoring is present (i.e., the data set is censored at the maximum
  value present).  The default is \code{FALSE}.} 
} \value{
  \code{exptol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{lambda.hat}{The mean of the data (i.e., \code{1/rate}).}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{Exponential}}
}

\references{
  Blischke, W. R. and Murthy, D. N. P. (2000), \emph{Reliability: Modeling, Prediction, and Optimization},
  John Wiley & Sons, Inc.
} 

\examples{ 
## 95\%/99\% 1-sided exponential tolerance intervals for a
## sample of size 50. 

set.seed(100)
x <- rexp(100, 0.004)
out <- exptol.int(x = x, alpha = 0.05, P = 0.99, side = 1,
                  type.2 = FALSE)
out

plottol(out, x, plot.type = "both", side = "lower", 
        x.lab = "Exponential Data")
}

\keyword{file}



