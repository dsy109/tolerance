\name{DiscretePareto}
\alias{DiscretePareto}
\alias{ddpareto}
\alias{pdpareto}
\alias{qdpareto}
\alias{rdpareto}
\title{
Discrete Pareto Distribution
}
\description{
Density (mass), distribution function, quantile function, and random generation for the discrete Pareto distribution. 
}
\usage{
ddpareto(x, theta, log = FALSE)
pdpareto(q, theta, lower.tail = TRUE, log.p = FALSE)
qdpareto(p, theta, lower.tail = TRUE, log.p = FALSE)
rdpareto(n, theta)
}

\arguments{
    \item{x, q}{Vector of quantiles.}
    \item{p}{Vector of probabilities.}
    \item{n}{The number of observations.  If \code{length>1}, then the length is taken to be the number
    required.}
    \item{theta}{The shape parameter, which must be greater than 0 and less than 1.}
    \item{log, log.p}{Logical vectors.  If \code{TRUE}, then the probabilities are given as \code{log(p)}.}
    \item{lower.tail}{Logical vector.  If \code{TRUE}, then probabilities are \eqn{P[X\le x]}, else \eqn{P[X>x]}.}
}
\details{
The discrete Pareto distribution has mass
\deqn{p(x) = \theta^{\log(1+x)}-\theta^{\log(2+x)},}{%
      p(x) = \theta^(\log(1+x))-\theta^(\log(2+x)),}
where \eqn{x=0,1,\ldots} and \eqn{0<\theta<1} is the shape parameter. 
}
\references{
Krishna, H. and Pundir, P. S. (2009), Discrete Burr and Discrete Pareto Distributions, 
\emph{Statistical Methodology}, \bold{6}, 177--188.

Young, D. S., Naghizadeh Qomi, M., and Kiapour, A. (2019), Approximate Discrete Pareto Tolerance Limits for Characterizing Extremes in Count Data, \emph{Statistica Neerlandica}, \bold{73}, 4--21. 
}
\value{
\code{ddpareto} gives the density (mass), \code{pdpareto} gives the distribution function, \code{qdpareto} gives the quantile function, and \code{rdpareto} generates random deviates for the specified distribution. 
}
\seealso{
\code{\link{runif}} and \code{\link{.Random.seed}} about random number generation.
}
\examples{
## Randomly generated data from the discrete Pareto
## distribution.

set.seed(100)
x <- rdpareto(n = 150, theta = 0.2)
hist(x, main = "Randomly Generated Data", prob = TRUE)

x.1 <- sort(x)
y <- ddpareto(x = x.1, theta = 0.2)
lines(x.1, y, col = 2, lwd = 2)

plot(x.1, pdpareto(q = x.1, theta = 0.2), type = "l", 
     xlab = "x", ylab = "Cumulative Probabilities")

qdpareto(p = 0.80, theta = 0.2, lower.tail = FALSE)
qdpareto(p = 0.95, theta = 0.2)
}
\keyword{file}
