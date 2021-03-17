\name{regtol.int}
\title{(Multiple) Linear Regression Tolerance Bounds}
\alias{regtol.int}
\usage{
regtol.int(reg, new.x = NULL, side = 1, alpha = 0.05, P = 0.99) 
}
\description{
  Provides 1-sided or 2-sided (multiple) linear regression tolerance bounds.  It is also possible to fit a 
  regression through the origin model.
}

\arguments{
  \item{reg}{An object of class \code{lm} (i.e., the results from a linear regression routine).}
  \item{new.x}{An optional data frame in which to look for variables with which to predict. If omitted, the fitted values are used.}
  \item{side}{Whether a 1-sided or 2-sided tolerance bound is required (determined by \code{side = 1} or \code{side = 2},
  respectively).} 
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by the tolerance bound(s).}
} 
\value{
  \code{regtol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by the tolerance bound(s).}
  \item{y}{The value of the response given on the left-hand side of the model in \code{reg}.}
  \item{y.hat}{The predicted value of the response for the fitted linear regression model.  This data frame is sorted by
  this value.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}} 

\references{
  Wallis, W. A. (1951), Tolerance Intervals for Linear Regression, in \emph{Second Berkeley Symposium on Mathematical
  Statistics and Probability}, ed. J. Neyman, Berkeley: University of CA Press, 43--51.

  Young, D. S. (2013), Regression Tolerance Intervals, \emph{Communications in Statistics - Simulation and Computation}, \bold{42}, 2040--2055.
} 

\seealso{
  \code{\link{lm}}
}

\examples{ 
## 95\%/95\% 2-sided linear regression tolerance bounds
## for a sample of size 100. 

set.seed(100)
x <- runif(100, 0, 10)
y <- 20 + 5*x + rnorm(100, 0, 3)
out <- regtol.int(reg = lm(y ~ x), new.x = data.frame(x = c(3, 6, 9)), 
                  side = 2, alpha = 0.05, P = 0.95)
out

plottol(out, x = cbind(1, x), y = y, side = "two", x.lab = "X", 
        y.lab = "Y")

}

\keyword{file}






