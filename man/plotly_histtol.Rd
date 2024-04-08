\name{plotly_histtol}
\alias{plotly_histtol}

\title{
Plotting Histograms and Corresponding Tolerance Intervals for Continuous Data
}
\description{
Provides interactive tolerance intervals for continous data based on its histogram.
}
\usage{
plotly_histtol(tol.out, 
               x,  
               side = c("two","upper", "lower"), 
               x.lab = NULL,
               x.lab.size = NULL,
               x.tick.size = NULL,
               y.lab.size = NULL,
               y.tick.size = NULL,
               title = NULL,
               title.size = NULL,
               title.position.x = NULL,
               title.position.y = NULL,
               bin.col = NULL,
               tol.col = NULL,
               tol.lwd = NULL,
               tol.line.type = c("dash","dot","dashdot","solid"))
}

\arguments{
  \item{tol.out}{Output from any continuous tolerance interval procedure.}
  \item{x}{Data from a continuous distribution.}
  \item{side}{\code{side = "two"} produces plots for either the two-sided tolerance intervals or both one-sided tolerance intervals. This will be determined by the output in \code{tol.out}. \code{side = "upper"} produces plots showing the upper tolerance bounds.  \code{side = "lower"} produces plots showing the lower tolerance bounds. Note that if the output of in \code{tol.out} shows 2-sided tolerance intervals, \code{side = "upper"} and \code{side = "lower"} still shows both upper AND lower tolerance intervals.}
  \item{x.lab}{Label of the x-axis.}
  \item{x.lab.size}{Size of label of the x-axis.}
  \item{x.tick.size}{Size of tick marks on the x-axis.}
  \item{y.lab.size}{Size of label of the y-axis.}
  \item{y.tick.size}{Size of tick marks on the y-axis.}
  \item{title}{The main title on top of the histogram.}
  \item{title.size}{Size of the title.}
  \item{title.position.x}{Horizontal position of the title.}
  \item{title.position.y}{Vertical position of the title.}
  \item{bin.col}{Color of the bins.}
  \item{tol.col}{Color of the tolerance interval(s).}
  \item{tol.lwd}{Width of the tolerance interval(s).}
  \item{tol.line.type}{Line type of the tolerance interval(s).}
}
\value{
 \code{plotly_histtol} can return histograms as well as corresponding tolerance intervals for any continuous data.
}


\seealso{
 \code{\link{plottol}}
}

\references{
  Montgomery, D. C. (2005), \emph{Introduction to Statistical Quality Control}, Fifth Edition, John Wiley & Sons, Inc.
}

\examples{
## 90%/90% 1-sided Weibull tolerance intervals for a sample
## of size 150.
set.seed(100)
x <- rweibull(150, 3, 75)
out <- exttol.int(x = x, alpha = 0.15, P = 0.90, dist = "Weibull" , side = 1)
out
plotly_histtol(out, x, side = "lower", x.lab = "Weibull Data" , tol.lwd = 3)
}