\name{dparetotol.int}
\alias{dparetotol.int}
\title{
Discrete Pareto Tolerance Intervals
}
\description{
Provides 1-sided or 2-sided tolerance intervals for data distributed according to the discrete Pareto distribution. 
}
\usage{
dparetotol.int(x, m = NULL, alpha = 0.05, P = 0.99, side = 1, 
                ...) 
}
\arguments{
    \item{x}{A vector of raw data which is distributed according to a discrete Pareto distribution.}
	\item{m}{The number of observations in a future sample for which the tolerance limits will be calculated.  By default, \code{m = NULL} and, thus, \code{m} will be set equal to the original sample size.}
    \item{alpha}{The level chosen such that 1-alpha is the confidence level.}
    \item{P}{The proportion of the population to be covered by this tolerance interval.}
    \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2}, respectively).}
    \item{...}{Additional arguments passed to the \code{dpareto.ll} function, which is used for maximum likelihood estimation.}
}
\details{
The discrete Pareto is a discretized of the continuous Type II Pareto distribution (also called the Lomax distribution). Discrete Pareto distributions are heavily right-skewed distributions and potentially good models for discrete lifetime data and extremes in count data.  For most practical applications, one will typically be interested in 1-sided upper bounds.
}
\references{
Young, D. S., Naghizadeh Qomi, M., and Kiapour, A. (2019), Approximate Discrete Pareto Tolerance Limits for Characterizing Extremes in Count Data, \emph{Statistica Neerlandica}, \bold{73}, 4--21. 
}
\value{
\code{dparetotol.int} returns a data frame with the following items:
		\item{\code{alpha}}{The specified significance level.} 
		\item{\code{P}}{The proportion of the population covered by this tolerance interval.} 
		\item{\code{theta}}{MLE for the shape parameter \code{theta}.} 
		\item{\code{1-sided.lower}}{The 1-sided lower tolerance bound. This is given only if \code{side = 1.}} 
		\item{\code{1-sided.upper}}{The 1-sided upper tolerance bound. This is given only if \code{side = 1.}} 
		\item{\code{2-sided.lower}}{The 2-sided lower tolerance bound. This is given only if \code{side = 2.}} 
		\item{\code{2-sided.upper}}{The 2-sided upper tolerance bound. This is given only if \code{side = 2.}} 				
}
\seealso{
\code{\link{DiscretePareto}}, \code{\link{dpareto.ll}}
}
\examples{
## 95\%/95\% 1-sided tolerance intervals for data assuming 
## the discrete Pareto distribution.

set.seed(100)

x <- rdpareto(n = 500, theta = 0.5)
out <- dparetotol.int(x, alpha = 0.05, P = 0.95, side = 1)
out
}
\keyword{file}
