\name{plotly_anovatol}
\alias{plotly_anovatol}

\title{
Plotting Tolerance Intervals for ANOVA
}
\description{
Plot tolerance intervals for each factor level in a balanced (or nearly-balanced) ANOVA
}
\usage{
plotly_anovatol(tol.out,
                x, 
                factors = NULL, 
                side = c("two","upper", "lower"),
                range.min = NULL,
                range.max = NULL,
                x.lab = NULL,
                x.lab.size = NULL,
                y.lab = NULL,
                y.lab.size = NULL,
                x.tick.size = NULL,
                y.tick.size = NULL,
                x.col = NULL,
                x.cex = NULL,
                tol.col = NULL,
                tol.lwd = NULL,
                tol.line.type = c("dash","dot","dashdot","solid"),
                tol.lower.col = NULL,
                tol.lower.lwd = NULL,
                tol.lower.line.type = c("dash","dot","dashdot","solid"),
                tol.upper.col = NULL,
                tol.upper.lwd = NULL,
                tol.upper.line.type = c("dash","dot","dashdot","solid"),
                title = NULL,
                title.position.x = NULL,
                title.position.y = NULL,
                title.size = NULL)
}

\arguments{
  \item{tol.out}{Output from any ANOVA tolerance interval procedure.}
  \item{x}{A data frame consisting of the data fitted in lm.out. Note that data must have
one column for each main effect (i.e., factor) that is analyzed in lm.out and that
these columns must be of class factor.}
  \item{factors}{Specify certain factor(s) to present. The name(s) of the factor(s) needs to be consistent with the name(s) in the original dataset.}
  \item{side}{\code{side = "two"} produces plots for either the two-sided tolerance intervals or both one-sided tolerance intervals. This will be determined by the output in \code{tol.out}. \code{side = "upper"} produces plots showing the upper tolerance bounds.  \code{side = "lower"} produces plots showing the lower tolerance bounds. Note that if the output of in \code{tol.out} shows 2-sided tolerance intervals, \code{side = "upper"} and \code{side = "lower"} still shows both upper AND lower tolerance intervals.}
  \item{range.min}{Minimum value on the y-axis. If actual lower limit is greater than \code{range.min}, then the lower limit will be presented.}
  \item{range.max}{Maximum value on the y-axis. If actual upper limit is smaller than \code{range.max}, then the upper limit will be presented.}
  \item{x.lab}{Label of the x-axis.}
  \item{x.lab.size}{Size of label of the x-axis.}
  \item{y.lab}{Label of the y-axis.}
  \item{y.lab.size}{Size of label of the y-axis.}
  \item{x.tick.size}{Size of tick marks on the x-axis.}
  \item{y.tick.size}{Size of tick marks on the y-axis.}
  \item{x.col}{Color of original data points.}                          
  \item{x.cex}{Size of original data points.}
  \item{tol.col}{Color of the tolerance intervals when \code{tol.out} shows 2-sided tolerance intervals.}
  \item{tol.lwd}{Width of the tolerance intervals when \code{tol.out} shows 2-sided tolerance intervals.}
  \item{tol.line.type}{Line type of the tolerance intervals when \code{tol.out} shows 2-sided tolerance intervals.}
  \item{tol.lower.col}{Color of the lower tolerance interval when \code{tol.out} shows 1-sided tolerance intervals. When \code{side = "two"}, users still have options to choose different colors for upper and lower tolerance intervals.}
  \item{tol.lower.lwd}{Width of the lower tolerance interval when \code{tol.out} shows 1-sided tolerance intervals. When \code{side = "two"}, users still have options to choose different widths for upper and lower tolerance intervals.}
  \item{tol.lower.line.type}{Line type of lower tolerance interval when \code{tol.out} shows 1-sided tolerance intervals. When \code{side = "two"}, users still have options to choose different widths for upper and lower tolerance intervals.}
  \item{tol.upper.col}{Color of the upper tolerance interval when \code{tol.out} shows 1-sided tolerance intervals. When \code{side = "two"}, users still have options to choose different colors for upper and lower tolerance intervals.}
  \item{tol.upper.lwd}{Width of the upper tolerance interval when \code{tol.out} shows 1-sided tolerance intervals. When \code{side = "two"}, users still have options to choose different widths for upper and lower tolerance intervals.}
  \item{tol.upper.line.type}{Line type of upper tolerance interval when \code{tol.out} shows 1-sided tolerance intervals. When \code{side = "two"}, users still have options to choose different widths for upper and lower tolerance intervals.}
 \item{title}{The main title on top of the plot}
 \item{title.position.x}{Horizontal position of the title.}
 \item{title.position.y}{Vertical position of the title.}
 \item{title.size}{Size of the title.}
}

\value{
 \code{plotly_anovatol} returns box plots as well as corresponding tolerance intervals for each main effect of an ANOVA.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{anovatol.int}}, \code{\link{plottol}}, \code{\link{K.factor}}, 
 \code{\link{normtol.int}}, \code{\link{lm}}, \code{\link{anova}}
}

\references{
  Howe, W. G. (1969), Two-Sided Tolerance Limits for Normal Populations - Some Improvements, \emph{Journal of the American Statistical Association}, \bold{64}, 610--620.
  
  Weissberg, A. and Beatty, G. (1969), Tables of Tolerance Limit Factors for Normal Distributions, \emph{Technometrics},
  \bold{2}, 483--500.
} 

\examples{
## 90%/95% 1-sided tolerance intervals for a 2-way ANOVA
## using the "warpbreaks" data.
attach(warpbreaks)
lm.out <- lm(breaks ~ wool + tension)
out.1 <- anovatol.int(lm.out, data = warpbreaks, alpha = 0.10,
                    P = 0.95, side = 1, method = "HE")
out.1
plotly_anovatol(out.1, x = warpbreaks , factors = 'wool' , x.lab = "Wool" , side="two")

## 90%/95% 2-sided tolerance intervals for a 2-way ANOVA
## using the "warpbreaks" data.
out.2 <- anovatol.int(lm.out, data = warpbreaks, alpha = 0.10,
                      P = 0.95, side = 2, method = "HE")
out.2
plotly_anovatol(out.2, x = warpbreaks , range.min = 20 , range.max = 60)
}