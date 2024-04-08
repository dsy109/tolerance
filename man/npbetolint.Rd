\name{npbetol.int}
\title{Nonparametric Beta-Expectation Tolerance Intervals}
\alias{npbetol.int}
\usage{
npbetol.int(x, Beta = 0.95, side = 1, upper = NULL, lower = NULL)
}
\description{
  Provides 1-sided or 2-sided nonparametric (i.e., distribution-free) beta-expectation tolerance intervals for any continuous
  data set.  These are equivalent to nonparametric prediction intervals based on order statistics.
}

\arguments{
  \item{x}{A vector of data which no distributional assumptions are made.  The data is only assumed to come from a continuous
  distribution.}
  \item{Beta}{The confidence level.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{upper}{The upper bound of the data.  When \code{NULL}, then the maximum of \code{x} is used.}
  \item{lower}{The lower bound of the data.  When \code{NULL}, then the minimum of \code{x} is used.}
} \value{
  \code{nptol.int} returns a data frame with items:
  \item{Beta}{The specified confidence level.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{distfree.est}}, \code{\link{npregtol.int}}, \code{\link{nptol.int}}
}

\references{
  Beran, R. and Hall, P. (1993), Interpolated Nonparametric Prediction Intervals and Confidence Intervals, \emph{Journal of the Royal Statistical Society, Series B}, \bold{55}, 643--652.
} 

\examples{ 
## Nonparametric 90\%-expectation tolerance intervals 
## for a sample of size 100. 

set.seed(100)
x <- rexp(100, 5)
out <- npbetol.int(x = x, Beta = 0.90, side = 2,
                   upper = NULL, lower = NULL)
out
}

\keyword{file}




