\name{ggplottol.control}
\alias{ggplottol.control}

\title{
Plotting Tolerance Intervals for Control Charts
}
\description{
Provides interactive control charts for tolerance bounds on continuous data.
}
\usage{
ggplottol.control(tol.out, x, side = c("two", "upper", "lower"),
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
  \item{x.col}{Color of original data points.}                          
  \item{x.cex}{Size of original data points.}
  \item{fit.col}{Color of fitted line.}
  \item{fit.lwd}{Width of fitted line.}
  \item{tol.col}{Color of the tolerance intervals when \code{tol.out} shows 2-sided tolerance intervals.}
  \item{tol.lwd}{Width of the tolerance intervals when \code{tol.out} shows 2-sided tolerance intervals.}
  \item{title.position.x}{Horizontal position of the title.}
  \item{title.position.y}{Vertical position of the title.}
  \item{title.size}{Size of the title.}
}
\value{
 \code{ggplottol.control} can return boxplots as well as corresponding tolerance intervals for any continuous data.
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
ggplottol.control(out, x, side = "lower", x.lab = "Weibull Data")
}