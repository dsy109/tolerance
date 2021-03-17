\name{dpareto.ll}
\alias{dpareto.ll}
\title{
Maximum Likelihood Estimation for the Discrete Pareto Distribution
}
\description{
Performs maximum likelihood estimation for the parameter of the discrete Pareto distribution. 
}
\usage{
dpareto.ll(x, theta = NULL, ...) 
}
\arguments{
    \item{x}{A vector of raw data which is distributed according to a Poisson-Lindley distribution.}
    \item{theta}{Optional starting value for the parameter.  If \code{NULL}, then the method of moments estimator is used.}
    \item{...}{Additional arguments passed to the \code{mle} function.}
}
\details{
The discrete Pareto distribution is a discretized of the continuous Type II Pareto distribution (also called the Lomax distribution).
}
\references{
Krishna, H. and Pundir, P. S. (2009), Discrete Burr and Discrete Pareto Distributions, 
\emph{Statistical Methodology}, \bold{6}, 177--188.

Young, D. S., Naghizadeh Qomi, M., and Kiapour, A. (2019), Approximate Discrete Pareto Tolerance Limits for Characterizing Extremes in Count Data, \emph{Statistica Neerlandica}, \bold{73}, 4--21. 
}
\value{
See the help file for \code{mle} to see how the output is structured.				
}
\seealso{
\code{\link{mle}}, \code{\link{DiscretePareto}}
}
\examples{
## Maximum likelihood estimation for randomly generated data
## from the discrete Pareto distribution. 

set.seed(100)

dp.data <- rdpareto(n = 500, theta = 0.2)
out.dp <- dpareto.ll(dp.data)
stats4::coef(out.dp)
stats4::vcov(out.dp)
}
\keyword{file}
