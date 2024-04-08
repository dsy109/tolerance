\name{plotly_multitol}
\alias{plotly_multitol}

\title{
Plotting Tolerance Region for Multivariate Distributions
}
\description{
Provides interactive tolerance region on multivariate continuous data.
}
\usage{
plotly_multitol(tol.out,
               x,
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
               tol.col = NULL,
               tol.lwd = NULL,
               tol.line.type = c("dash","dot","dashdot","solid"),
               title = NULL,
               title.position.x = NULL,
               title.position.y = NULL,
               title.size = NULL)
}

\arguments{
  \item{tol.out}{Output from \code{mvtol.region} for multivariate data.}
  \item{x}{Multivariate data from continuous distributions.}
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
  \item{tol.col}{Color of the tolerance region.}
  \item{tol.lwd}{Width of boundary of the tolerance region when data is bivariate.}
  \item{tol.line.type}{Line type of the tolerance region for bivariate data.}
  \item{title}{The main title on top of the plot.}
  \item{title.size}{Size of the title.}
  \item{title.position.x}{Horizontal position of the title.}
  \item{title.position.y}{Vertical position of the title.}
}
\value{
 \code{plotly_multitol} returns tolerance regions for both bivariate and trivariate continuous data. 
}

\seealso{
 \code{\link{plottol}}, \code{\link{mvtol.region}}
}

\references{
  Krishnamoorthy, K. and Mathew, T. (1999), Comparison of Approximation Methods for Computing Tolerance Factors for a Multivariate Normal Population, 
  \emph{Technometrics}, \bold{41}, 234--249.

  Krishnamoorthy, K. and Mondal, S. (2006), Improved Tolerance Factors for Multivariate Normal Distributions, 
  \emph{Communications in Statistics - Simulation and Computation}, \bold{35}, 461--478.
} 

\examples{
## 90%/90% bivariate normal tolerance region.
set.seed(100)
x1 <- rnorm(100, 0, 0.2)
x2 <- rnorm(100, 0, 0.5)
x <- cbind(x1, x2)
out1 <- mvtol.region(x = x, alpha = 0.10, P = 0.90, B = 1000,
                     method = "KM")
out1
plotly_multitol(out1, x , x.lab = "X1" , y.lab = "X2")

## 90%/90% trivariate normal tolerance region.
set.seed(100)
x1 <- rnorm(100, 0, 0.2)
x2 <- rnorm(100, 0, 0.5)
x3 <- rnorm(100, 5, 1)
x <- cbind(x1, x2, x3)
mvtol.region(x = x, alpha = c(0.10, 0.05, 0.01),
             P = c(0.90, 0.95, 0.99), B = 1000, method = "KM")
out2 <- mvtol.region(x = x, alpha = 0.10, P = 0.90, B = 1000,
                     method = "KM")
out2
plotly_multitol(out2, x , x.lab = "X1" , y.lab = "X2" , z.lab = "X3",
                title.position.x = 0.57)
}