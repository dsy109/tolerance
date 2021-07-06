\name{ggplottol.hist}
\alias{ggplottol.hist}

\title{
Plotting Histograms and Corresponding Tolerance Intervals for Continuous Data
}
\description{
Provides interactive tolerance intervals for continous data based on its histogram.
}
\usage{
ggplottol.hist(tol.out, x, side = c("two", "upper", "lower"),
                  x.lab = NULL, x.lab.size = NULL,
                  y.lab = NULL, y.lab.size = NULL,
                  x.tick.size = NULL, y.tick.size = NULL,
                  x.col = NULL, x.cex = NULL,
                  fit.col = NULL, fit.lwd = NULL,
                  tol.col = NULL, tol.lwd = NULL,
                  title.position.x = NULL, title.position.y = NULL, title.size = NULL)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{title.position.x}{Horizontal position of the title.}
  \item{title.position.y}{Vertical position of the title.}
  \item{title.size}{Size of the title.}
  \item{n.xlab}{Number of tick marks on the x-axis.}
  \item{n.bin}{Number of bins of the histogram.}
  \item{binwidth}{Width of bins of the histogram.}
  \item{bin.fill}{Color of the bins of the histogram.}
  \item{bin.col}{Color of the border of the bins.}
  \item{bin.alpha}{Degree of transparency of bins.}
  \item{tol.col}{Color of the tolerance interval(s).}
  \item{tol.lwd}{Width of the tolerance interval(s).}
}
\value{
 \code{ggplottol.hist} can return histograms as well as corresponding tolerance intervals for any continuous data.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code\link{plottol}
}
\examples{
## 90%/90% 1-sided Weibull tolerance intervals for a sample
## of size 150.
set.seed(100)
x <- rweibull(150, 3, 75)
out <- exttol.int(x = x, alpha = 0.15, P = 0.90, dist = "Weibull")
out
ggplottol.hist(out, x, side = "lower", x.lab = "Weibull Data")
}