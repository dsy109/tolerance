\name{nlregtol.int}
\title{Nonlinear Regression Tolerance Bounds}
\alias{nlregtol.int}
\usage{
nlregtol.int(formula, xy.data = data.frame(), x.new = NULL,
             side = 1, alpha = 0.05, P = 0.99, maxiter = 50, 
             ...)
}
\description{
  Provides 1-sided or 2-sided nonlinear regression tolerance bounds.
}

\arguments{
  \item{formula}{A nonlinear model formula including variables and parameters.}
  \item{xy.data}{A data frame in which to evaluate the formulas in \code{formula}.  The first column
  of \code{xy.data} must be the response variable.}
  \item{x.new}{Any new levels of the predictor(s) for which to report the tolerance bounds.  The number of columns must be
  1 less than the number of columns for \code{xy.data}.}
  \item{side}{Whether a 1-sided or 2-sided tolerance bound is required (determined by \code{side = 1} or \code{side = 2},
  respectively).} 
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by the tolerance bound(s).}
  \item{maxiter}{A positive integer specifying the maximum number of iterations that the nonlinear least squares routine (\code{nls})
  should run.}
  \item{...}{Optional arguments passed to \code{nls} when estimating the nonlinear regression equation.}
} 
\value{
  \code{nlregtol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by the tolerance bound(s).}
  \item{y.hat}{The predicted value of the response for the fitted nonlinear regression model.}
  \item{y}{The value of the response given in the first column of \code{xy.data}.  This data frame is sorted by
  this value.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}} 
\details{
  It is highly recommended that the user specify starting values for the \code{nls} routine.
}
\references{
  Wallis, W. A. (1951), Tolerance Intervals for Linear Regression, in \emph{Second Berkeley Symposium on Mathematical
  Statistics and Probability}, ed. J. Neyman, Berkeley: University of CA Press, 43--51.

  Young, D. S. (2013), Regression Tolerance Intervals, \emph{Communications in Statistics - Simulation and Computation}, \bold{42}, 2040--2055.
} 

\seealso{
  \code{\link{nls}}
}

\examples{ 
## 95\%/95\% 2-sided nonlinear regression tolerance bounds
## for a sample of size 50. 

set.seed(100)
x <- runif(50, 5, 45)
f1 <- function(x, b1, b2) b1 + (0.49 - b1)*exp(-b2*(x - 8)) +
               rnorm(50, sd = 0.01) 
y <- f1(x, 0.39, 0.11)
formula <- as.formula(y ~ b1 + (0.49 - b1)*exp(-b2*(x - 8)))
out <- nlregtol.int(formula = formula, 
                    xy.data = data.frame(cbind(y, x)), 
                    x.new=cbind(c(10, 20)), side = 2, 
                    alpha = 0.05, P = 0.95)
out

plottol(out, x = x, y = y, side = "two", x.lab = "X", 
         y.lab = "Y")
}

\keyword{file}





