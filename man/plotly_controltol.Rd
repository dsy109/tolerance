\name{plotly_controltol}
\alias{plotly_controltol}

\title{
Plotting Tolerance Intervals for Control Charts
}
\description{
Provides interactive control charts for tolerance bounds on continuous data.
}
\usage{
plotly_controltol(tol.out , 
                 x , 
                 side = c("two","upper", "lower"),
                 x.lab = NULL,
                 x.lab.size = NULL,
                 y.lab = NULL,
                 y.lab.size = NULL,
                 x.tick.size = NULL,
                 y.tick.size = NULL,
                 x.col = NULL,
                 x.cex = NULL,
                 fit.col = NULL,
                 fit.lwd = NULL,
                 fit.line.type = c("solid","dash","dot","dashdot"),
                 tol.col = NULL,
                 tol.lwd = NULL,
                 tol.line.type = c("dash","dot","dashdot","solid"),
                 title.position.x = NULL,
                 title.position.y = NULL,
                 title.size = NULL,
                 title = NULL)
}

\arguments{
  \item{tol.out}{Output from any continuous tolerance interval procedure.}
  \item{x}{Data from a continuous distribution.}
  \item{side}{\code{side = "two"} produces plots for either the two-sided tolerance intervals or both one-sided tolerance intervals. This will be determined by the output in \code{tol.out}. \code{side = "upper"} produces plots showing the upper tolerance bounds.  \code{side = "lower"} produces plots showing the lower tolerance bounds. Note that if the output of in \code{tol.out} shows 2-sided tolerance intervals, \code{side = "upper"} and \code{side = "lower"} still shows both upper AND lower tolerance intervals.}
  \item{x.lab}{Label of the x-axis.}
  \item{x.lab.size}{Size of label of the x-axis.}
  \item{y.lab}{Label of the y-axis.}
  \item{y.lab.size}{Size of label of the y-axis.}
  \item{x.tick.size}{Size of tick marks on the x-axis.}
  \item{y.tick.size}{Size of tick marks on the y-axis.}
  \item{x.col}{Color of original data points.}                          
  \item{x.cex}{Size of original data points.}
  \item{fit.col}{Color of fitted line.}
  \item{fit.lwd}{Width of fitted line.}
  \item{fit.line.type}{Type of the fitted line.}
  \item{tol.col}{Color of the tolerance intervals when \code{tol.out} shows 2-sided tolerance intervals.}
  \item{tol.lwd}{Width of the tolerance intervals when \code{tol.out} shows 2-sided tolerance intervals.}
  \item{tol.line.type}{Line type of tolerance intervals.}
  \item{title}{The main title on top of the plot.}
  \item{title.size}{Size of the title.}
  \item{title.position.x}{Horizontal position of the title.}
  \item{title.position.y}{Vertical position of the title.}
}
\value{
 \code{plotly_controltol} can return boxplots as well as corresponding tolerance intervals for any continuous data.
}

\seealso{
 \code{\link{plottol}}
}

\references{
  Montgomery, D. C. (2005), \emph{Introduction to Statistical Quality Control}, Fifth Edition, John Wiley & Sons, Inc.
}

\examples{
## 95%/85% 2-sided Bayesian normal tolerance limits for
## a sample of size 100.
set.seed(100)
x <- rnorm(100)
out <- bayesnormtol.int(x = x, alpha = 0.05, P = 0.85,
                        side = 2, method = "EXACT",
                        hyper.par = list(mu.0 = 0,
                                         sig2.0 = 1, n.0 = 10, m.0 = 10))
out
plotly_controltol(out, x, x.lab = "Normal Data")
}