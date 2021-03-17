\name{npregtol.int}
\title{Nonparametric Regression Tolerance Bounds}
\alias{npregtol.int}
\usage{
npregtol.int(x, y, y.hat, side = 1, alpha = 0.05, P = 0.99,
             method = c("WILKS", "WALD", "HM"), upper = NULL, 
             lower = NULL)
}
\description{
  Provides 1-sided or 2-sided nonparametric regression tolerance bounds.
}

\arguments{
  \item{x}{A vector of values for the predictor variable.  Currently, this function is only capable of handling
  a single predictor.}
  \item{y}{A vector of values for the response variable.}
  \item{y.hat}{A vector of fitted values extracted from a nonparametric smoothing routine.}
  \item{side}{Whether a 1-sided or 2-sided tolerance bound is required (determined by \code{side = 1} or \code{side = 2},
  respectively).} 
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by the tolerance bound(s).}
  \item{method}{The method for determining which indices of the ordered residuals will be used for the tolerance bounds.  
  \code{"WILKS"}, \code{"WALD"}, and \code{"HM"} are each described in \code{\link{nptol.int}}.  However, since only one tolerance
  bound can actually be reported for this procedure, only the first tolerance bound will be returned.  Note that this is not
  an issue when \code{method = "WILKS"} is used as it only produces one set of tolerance bounds.}
  \item{upper}{The upper bound of the data.  When \code{NULL}, then the maximum of \code{x} is used.}
  \item{lower}{The lower bound of the data.  When \code{NULL}, then the minimum of \code{x} is used.}
} 
\value{
  \code{npregtol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by the tolerance bound(s).}
  \item{x}{The values of the predictor variable.}
  \item{y}{The values of the response variable.}
  \item{y.hat}{The predicted value of the response for the fitted nonparametric smoothing routine.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}} 

\seealso{
  \code{\link{loess}}, \code{\link{nptol.int}}, \code{\link{spline}}
}

\references{
  Young, D. S. (2013), Regression Tolerance Intervals, \emph{Communications in Statistics - Simulation and Computation}, \bold{42}, 2040--2055.
}

\examples{ 
## 95\%/95\% 2-sided nonparametric regression tolerance bounds
## for a sample of size 50. 

set.seed(100)
x <- runif(50, 5, 45)
f1 <- function(x, b1, b2) b1 + (0.49 - b1)*exp(-b2*(x - 8)) +
               rnorm(50, sd = 0.01) 
y <- f1(x, 0.39, 0.11)
y.hat <- loess(y~x)$fit
out <- npregtol.int(x = x, y = y, y.hat = y.hat, side = 2, 
                    alpha = 0.05, P = 0.95, method = "WILKS")
out

plottol(out, x = x, y = y, y.hat = y.hat, side = "two", 
        x.lab = "X", y.lab = "Y")
}

\keyword{file}






