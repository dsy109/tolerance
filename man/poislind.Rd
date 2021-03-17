\name{PoissonLindley}
\alias{PoissonLindley}
\alias{dpoislind}
\alias{ppoislind}
\alias{qpoislind}
\alias{rpoislind}
\title{
Discrete Poisson-Lindley Distribution
}
\description{
Density (mass), distribution function, quantile function, and random generation for the Poisson-Lindley distribution. 
}
\usage{
dpoislind(x, theta, log = FALSE)
ppoislind(q, theta, lower.tail = TRUE, log.p = FALSE)
qpoislind(p, theta, lower.tail = TRUE, log.p = FALSE)
rpoislind(n, theta)
}

\arguments{
    \item{x, q}{Vector of quantiles.}
    \item{p}{Vector of probabilities.}
    \item{n}{The number of observations.  If \code{length>1}, then the length is taken to be the number
    required.}
    \item{theta}{The shape parameter, which must be greater than 0.}
    \item{log, log.p}{Logical vectors.  If \code{TRUE}, then the probabilities are given as \code{log(p)}.}
    \item{lower.tail}{Logical vector.  If \code{TRUE}, then probabilities are \eqn{P[X\le x]}, else \eqn{P[X>x]}.}
}
\details{
The Poisson-Lindley distribution has mass
\deqn{p(x) = \frac{\theta^{2}(x + \theta + 2)}{(\theta + 1)^{x+3}},}{%
      p(x) = (\theta^2(x + \theta + 2))/(\theta + 1)^(x+3),}
where \eqn{x=0,1,\ldots} and \eqn{\theta>0} is the shape parameter. 
}
\references{
Ghitany, M. E. and Al-Mutairi, D. K. (2009), Estimation Methods for the Discrete Poisson-Lindley Distribution, 
\emph{Journal of Statistical Computation and Simulation}, \bold{79}, 1--9.

Sankaran, M. (1970), The Discrete Poisson-Lindley Distribution, \emph{Biometrics}, \bold{26}, 145--149. 
}
\value{
\code{dpoislind} gives the density (mass), \code{ppoislind} gives the distribution function, \code{qpoislind} gives the quantile function, and \code{rpoislind} generates random deviates for the specified distribution. 
}
\seealso{
\code{\link{runif}} and \code{\link{.Random.seed}} about random number generation.
}
\examples{
## Randomly generated data from the Poisson-Lindley
## distribution.

set.seed(100)
x <- rpoislind(n = 150, theta = 0.5)
hist(x, main = "Randomly Generated Data", prob = TRUE)

x.1 <- sort(x)
y <- dpoislind(x = x.1, theta = 0.5)
lines(x.1, y, col = 2, lwd = 2)

plot(x.1, ppoislind(q = x.1, theta = 0.5), type = "l", 
     xlab = "x", ylab = "Cumulative Probabilities")

qpoislind(p = 0.20, theta = 0.5, lower.tail = FALSE)
qpoislind(p = 0.80, theta = 0.5)
}
\keyword{file}
