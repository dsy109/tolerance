\name{plotly_regtol}
\alias{plotly_regtol}

\title{
Plotting Tolerance Intervals for Regressions
}
\description{
Provides interactive tolerance intervals for regression data. More specifically, \code{plotly_regtol} presents tolerance bounds for linear regression, nonlinear regression, and nonparametric regression models. In addtion, this updated function is capable of showing tolerance plane for trivariate regression models. 
}
\usage{
plotly_regtol(tol.out,
              x,
              new.x = NULL,
              y,
              side = c("two","upper", "lower"),
              rect = FALSE,
              smooth = 4,
              x.lab = NULL,
              x.lab.size = NULL,
              y.lab = NULL,
              y.lab.size = NULL,
              z.lab = NULL,
              z.lab.size = NULL,
              x.tick.size = NULL,
              y.tick.size = NULL,
              z.tick.size = NULL,
              x.col = NULL,
              x.cex = NULL,
              fit.col = NULL,
              fit.lwd = NULL,
              fit.line.type = c("dash","dot","dashdot","solid"),
              fit.opacity = NULL,
              tol.col = NULL,
              tol.lwd = NULL,
              tol.line.type = c("dash","dot","dashdot","solid"),
              tol.opacity = NULL,
              title.position.x = NULL,
              title.position.y = NULL,
              title = NULL,
              title.size = NULL)
}

\arguments{
  \item{tol.out}{Output from \code{regtol.int}, \code{nlregtol.int}, \code{npregtol.int} or \code{mvregtol.region} for regressional data.}
  \item{x}{Data frame for explanatory variables. If there are more than one explanatory variables, columns of \code{x} represents regressors.}
  \item{new.x}{An optional data frame in which to look for variables with which to predict. \code{new.x} can be used to plot linear regression, nonlinear regression, and multivariate linear regression. \code{new.x} has to be a subset of new data in the original output.}
  \item{y}{Data frame for response variable. \code{y} is in the formate of a vector.}
  \item{side}{\code{side = "two"} produces plots for either the two-sided tolerance intervals or both one-sided tolerance intervals. This will be determined by the output in \code{tol.out}. \code{side = "upper"} produces plots showing the upper tolerance bounds.  \code{side = "lower"} produces plots showing the lower tolerance bounds. Note that if the output of in \code{tol.out} shows 2-sided tolerance intervals, \code{side = "upper"} and \code{side = "lower"} still shows both upper AND lower tolerance intervals.}
  \item{rect}{This argument is used for plotting tolerance plane(s) of multivariate regression region. When \code{rect=TRUE} the x1-x2 plane is a rectangle. }
  \item{smooth}{The smooth parameter for the x1-x2 plane when \code{rect=TRUE}.}
  \item{x.lab}{Label of the x-axis.}
  \item{x.lab.size}{Size of label of the x-axis.}
  \item{y.lab}{Label of the y-axis.}
  \item{y.lab.size}{Size of label of the y-axis.}
  \item{z.lab}{Label of the z-axis.}
  \item{z.lab.size}{Size of label of the z-axis.}
  \item{x.tick.size}{Size of tick marks on the x-axis.}
  \item{y.tick.size}{Size of tick marks on the y-axis.}
  \item{z.tick.size}{Size of tick marks on the z-axis.}
  \item{x.col}{Color of original data points.}                          
  \item{x.cex}{Size of original data points.}
  \item{fit.col}{Color of fitted line or fitted plane.}
  \item{fit.lwd}{Width of fitted line or fitted plane.}
  \item{fit.line.type}{Type of fitted line or fitted plane.}
  \item{fit.opacity}{Opacity of fitted line or fitted plane.}
  \item{tol.col}{Color of tolerance intervals or tolerance plane.}
  \item{tol.lwd}{Width of tolerance intervals.}
  \item{tol.line.type}{Line type of tolerance intervals}
  \item{tol.opacity}{Opacity of tolerance region.}
  \item{title.position.x}{Horizontal position of the title.}
  \item{title.position.y}{Vertical position of the title.}
  \item{title}{The main title on top of the plot.}
  \item{title.size}{Size of the title.}
}
\value{
 \code{plotly_regtol} returns tolerance intervals for linear regression, nonlinear regression, nonparametric regression, as well as tolerance planes for multivariate (multiple) linear regression models.
}

\seealso{
 \code{\link{plottol}}, \code{\link{regtol.int}}, \code{\link{regtol.int}}, \code{\link{nlregtol.int}},
 \code{\link{npregtol.int}}, \code{\link{npregtol.int}},\code{\link{mvregtol.region}}
}

\references{
  Montgomery, D. C. (2005), Introduction to Statistical Quality Control, Fifth Edition, \emph{John Wiley & Sons, Inc}.
} 

\examples{
## 95%/95% 1-sided linear regression tolerance bounds
## for a sample of size 100.

library(plotly)

set.seed(100)
x <- runif(100, 0, 10)
y <- 20 + 5*x + rnorm(100, 0, 3)
out1 <- regtol.int(reg = lm(y ~ x), new.x = c(3,6,20), new=TRUE ,
                    side = 1, alpha = 0.05, P = 0.95)
plotly_regtol(tol.out = out1 , x=x , y=y , new.x = c(6,20), side = "two" ,
                   fit.line.type = "dash" , tol.line.type = "solid")
########################
set.seed(100)
x1 <- runif(100, 0, 10)
x2 <- rpois(100 , 5)
y <- 20 + 5*x1 + 3*x2 + rnorm(100, 0, 3)
x1.new <- runif(10 , 0 , 10)
x2.new <- rpois(10 , 5)
out2 <- regtol.int(reg = lm(y ~ x1 + x2), new.x = cbind(x1.new , x2.new), new=TRUE,
                    side = 1, alpha = 0.05, P = 0.95)
plotly_regtol(tol.out = out2 , y=y , x=cbind(x1,x2) , new.x = cbind(x1.new , x2.new) , 
                   rect = TRUE , side = "two")
###########################
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
                      x.new=c(10,20,50), side = 2,
                      alpha = 0.05, P = 0.95 , new = TRUE)
plotly_regtol(tol.out = out1 , x=x , y=y , new.x = c(20,50) , side = "two",
                   fit.line.type = "dot")
###############

## 95%/95% 1-sided nonparametric regression tolerance bounds
## for a sample of size 50.
set.seed(100)
x <- runif(50, 5, 45)
f1 <- function(x, b1, b2) b1 + (0.49 - b1)*exp(-b2*(x - 8)) + rnorm(50, sd = 0.01)
y <- f1(x, 0.39, 0.11)
y.hat <- loess(y~x)$fit
out1 <- npregtol.int(x = x, y = y, y.hat = y.hat, side = 1,
                      alpha = 0.05, P = 0.95, method = "WILKS" , new = TRUE)
plotly_regtol(tol.out = out1 , x=x , y=y , side = "two" , fit.line.type = "dash")
############
set.seed(100)
x1 <- runif(50, 5, 45)
x2 <- rnorm(50 , 0 , 1)
f1 <- function(x1 , x2 , b1, b2) {b1 + (0.49 - b1)*exp(-b2*(x1 + x2 - 8)) + rnorm(50, sd = 0.01)}
y <- f1(x1 , x2 , 0.39, 0.11)
y.hat <- loess(y~ x1 + x2)$fit
out2 <- npregtol.int(x = cbind(x1 , x2), y = y, y.hat = y.hat, side = 1,
                      alpha = 0.05, P = 0.95, method = "WILKS" , new = TRUE)
plotly_regtol(tol.out = out2 , y=y , x=cbind(x1,x2) , 
                   rect = TRUE , smooth = 100 ,  side = "two")
}