\name{plottol}
\title{Plotting Capabilities for Tolerance Intervals}
\alias{plottol}
\usage{
plottol(tol.out, x, y = NULL, y.hat = NULL, 
        side = c("two", "upper", "lower"), 
        plot.type = c("control", "hist", "both"), 
        x.lab = NULL, y.lab = NULL, z.lab = NULL, ...)
}
\description{
  Provides control charts and/or histograms for tolerance bounds on continuous data as well as tolerance ellipses
  for data distributed according to bivariate and trivariate normal distributions.  Scatterplots with regression tolerance bounds
  and interval plots for ANOVA tolerance intervals may also be produced.
}

\arguments{
  \item{tol.out}{Output from any continuous (including ANOVA) tolerance interval procedure or from a regression tolerance bound procedure.}
  \item{x}{Either data from a continuous distribution or the predictors for a regression model.  If this is a design matrix
  for a linear regression model, then it must be in matrix form AND include a column of 1's if there is to be an intercept.  Note
  that multiple predictors are only allowed if considering polynomial regression.  If the output for \code{tol.out} concerns
  ANOVA tolerance intervals, then \code{x} must be a data frame.}
  \item{y}{The response vector for a regression setting.  Leave as \code{NULL} if not doing regression tolerance bounds.}
  \item{y.hat}{The fitted values from a nonparametric smoothing routine if plotting nonparametric regression tolerance bounds.  Otherwise,
  leave as \code{NULL}.}
  \item{side}{\code{side = "two"} produces plots for either the two-sided tolerance intervals or both one-sided tolerance intervals.
  This will be determined by the output in \code{tol.out}.  \code{side = "upper"} produces plots showing the upper tolerance
  bounds.  \code{side = "lower"} produces plots showing the lower tolerance bounds.}
  \item{plot.type}{\code{plot.type = "control"} produces a control chart of the data along with the tolerance bounds specified
  by \code{side}.  \code{plot.type = "hist"} produces a histogram of the data along with the tolerance bounds specified by
  \code{side}.  \code{plot.type = "both"} produces both the control chart and histogram.  This argument is ignored
  when plotting regression data.}
  \item{x.lab}{Specify the label for the x-axis.}
  \item{y.lab}{Specify the label for the y-axis.}
  \item{z.lab}{Specify the label for the z-axis.}
  \item{...}{Additional arguments passed to the plotting function used for the control charts or regression scatterplots.}
} 
\value{
  \code{plottol} can return a control chart, histogram, or both for continuous data along with the calculated tolerance intervals.
  For regression data, \code{plottol} returns a scatterplot along with the regression tolerance bounds.  For ANOVA output, \code{plottol}
  returns an interval plot for each factor.
}
\references{
  Montgomery, D. C. (2005), \emph{Introduction to Statistical Quality Control}, Fifth Edition, John Wiley & Sons, Inc.
}


\examples{ 
## 90\%/90\% 1-sided Weibull tolerance intervals for a sample
## of size 150. 

set.seed(100)
x <- rweibull(150, 3, 75)
out <- exttol.int(x = x, alpha = 0.15, P = 0.90, 
                  dist = "Weibull")
out

plottol(out, x, plot.type = "both", side = "lower", 
        x.lab = "Weibull Data")

## 90\%/90\% trivariate normal tolerance region. 

set.seed(100)
x1 <- rnorm(100, 0, 0.2)
x2 <- rnorm(100, 0, 0.5)
x3 <- rnorm(100, 5, 1)
x <- cbind(x1, x2, x3)
mvtol.region(x = x, alpha = c(0.10, 0.05, 0.01), 
             P = c(0.90, 0.95, 0.99), B = 1000) 

out2 <- mvtol.region(x = x, alpha = 0.10, P = 0.90, B = 1000) 
out2
plottol(out2, x)

## 95\%/95\% 2-sided linear regression tolerance bounds
## for a sample of size 100. 

set.seed(100)
x <- runif(100, 0, 10)
y <- 20 + 5*x + rnorm(100, 0, 3)
out3 <- regtol.int(reg = lm(y ~ x), new.x = data.frame(x = c(3, 6, 9)), 
                   side = 2, alpha = 0.05, P = 0.95)
plottol(out3, x = cbind(1, x), y = y, side = "two", x.lab = "X", 
        y.lab = "Y")

}

\keyword{file}






