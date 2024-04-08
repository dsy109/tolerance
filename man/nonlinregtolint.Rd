\name{nlregtol.int}
\title{Nonlinear Regression Tolerance Bounds, Version 2}
\alias{nlregtol.int}
\usage{
nlregtol.int(formula, xy.data = data.frame(), x.new = NULL, 
             side = 1, alpha = 0.05, P = 0.99, maxiter = 50, 
             new = FALSE, ...)
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
  \item{new}{When \code{new = TRUE}, the function shows updated version of outcomes.}
  \item{...}{Optional arguments passed to \code{nls} when estimating the nonlinear regression equation.}
} 
\value{
  \code{npregtol.int2} returns a list with items:
  \item{tol}{Data frame of original response varible \code{y}, fitted values \code{y.hat}, corresponding tolerance limits. The data frame is ordered by \code{X} values. If there are new data for prediction, predicted values are attached in the end.}
  \item{alpha.P.side}{Model specifications of critical level, content level and side.}
  \item{reg.type}{Type of regression model.}
  \item{model}{The linear regression model fitted.}
  \item{newdata}{X values of new data for prediction.}
  \item{xy.data.original}{Original data frame}
} 
\details{
  It is highly recommended that the user specify starting values for the \code{nls} routine.
}
\references{
  Wallis, W. A. (1951), Tolerance Intervals for Linear Regression, in \emph{Second Berkeley Symposium on Mathematical
  Statistics and Probability}, ed. J. Neyman, Berkeley: University of CA Press, 43--51.

  Young, D. S. (2013), Regression Tolerance Intervals, \emph{Communications in Statistics - Simulation and Computation}, \bold{42}, 2040--2055.
} 

\seealso{
  \code{\link{nls}}, \code{\link{nlregtol.int}}
}

\examples{ 
## 95%/95% 2-sided nonlinear regression tolerance bounds
## for a sample of size 50.
set.seed(100)
x <- runif(50, 5, 45)
f1 <- function(x, b1, b2) b1 + (0.49 - b1)*exp(-b2*(x - 8)) +
  rnorm(50, sd = 0.01)
y <- f1(x, 0.39, 0.11)
formula <- as.formula(y ~ b1 + (0.49 - b1)*exp(-b2*(x - 8)))
out1 <- nlregtol.int(formula = formula,
                     xy.data = data.frame(cbind(y, x)),
                     x.new=c(10,20), side = 2,
                     alpha = 0.05, P = 0.95 , new = TRUE)
out1
#########
set.seed(100)
x1 <- runif(50, 5, 45)
x2 <- rnorm(50, 0, 10)
f1 <- function(x1, x2, b1, b2) {(0.49 - b1)*exp(-b2*(x1 + x2 - 8)) +
    rnorm(50, sd = 0.01)}
y <- f1(x1 , x2 , 0.25 , 0.39)
formula <- as.formula(y ~ (0.49 - b1)*exp(-b2*(x1 + x2 - 8)))
out2 <- nlregtol.int(formula = formula,
                     xy.data = data.frame(cbind(y, x1 , x2)),
                     x.new=cbind(c(10,20) , c(47 , 53)), side = 2,
                     alpha = 0.05, P = 0.95 , new = TRUE)
out2
}

\keyword{file}