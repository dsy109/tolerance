\name{poislindtol.int}
\alias{poislindtol.int}
\title{
Poisson-Lindley Tolerance Intervals
}
\description{
Provides 1-sided or 2-sided tolerance intervals for data distributed according to the Poisson-Lindley distribution. 
}
\usage{
poislindtol.int(x, m = NULL, alpha = 0.05, P = 0.99, side = 1, 
                ...) 
}
\arguments{
    \item{x}{A vector of raw data which is distributed according to a Poisson-Lindley distribution.}
	\item{m}{The number of observations in a future sample for which the tolerance limits will be calculated.  By default, \code{m = NULL} and, thus, \code{m} will be set equal to the original sample size.}
    \item{alpha}{The level chosen such that 1-alpha is the confidence level.}
    \item{P}{The proportion of the population to be covered by this tolerance interval.}
    \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2}, respectively).}
    \item{...}{Additional arguments passed to the \code{poislind.ll} function, which is used for maximum likelihood estimation.}
}
\details{
The discrete Poisson-Lindley distribution is a compound distribution that, potentially, provides a better fit for count data relative to the traditional Poisson and negative binomial distributions. Poisson-Lindley distributions are heavily right-skewed distributions.  For most practical applications, one will typically be interested in 1-sided upper bounds.
}
\references{
Naghizadeh Qomi, M., Kiapour, A., and Young, D. S. (2015), Approximate Tolerance Intervals for the Discrete Poisson-Lindley Distribution, 
\emph{Journal of Statistical Computation and Simulation}, \bold{86}, 841--854.
}
\value{
\code{poislindtol.int} returns a data frame with the following items:
		\item{\code{alpha}}{The specified significance level.} 
		\item{\code{P}}{The proportion of the population covered by this tolerance interval.} 
		\item{\code{theta}}{MLE for the shape parameter \code{theta}.} 
		\item{\code{1-sided.lower}}{The 1-sided lower tolerance bound. This is given only if \code{side = 1.}} 
		\item{\code{1-sided.upper}}{The 1-sided upper tolerance bound. This is given only if \code{side = 1.}} 
		\item{\code{2-sided.lower}}{The 2-sided lower tolerance bound. This is given only if \code{side = 2.}} 
		\item{\code{2-sided.upper}}{The 2-sided upper tolerance bound. This is given only if \code{side = 2.}} 				
}
\seealso{
\code{\link{PoissonLindley}}, \code{\link{poislind.ll}}
}
\examples{
## 90\%/90\% 1-sided tolerance intervals for data assuming 
## the Poisson-Lindley distribution.

x <- c(rep(0, 447), rep(1, 132), rep(2, 42), rep(3, 21), 
       rep(4, 3), rep(5, 2))
out <- poislindtol.int(x, alpha = 0.10, P = 0.90, side = 1)
out
}
\keyword{file}
