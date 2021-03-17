\name{TwoParExponential}
\alias{TwoParExponential}
\alias{d2exp}
\alias{p2exp}
\alias{q2exp}
\alias{r2exp}

\title{The 2-Parameter Exponential Distribution}
\description{
  Density, distribution function, quantile function, and random generation for the 2-parameter 
  exponential distribution with rate equal to \code{rate} and shift equal to \code{shift}. 
}
\usage{
d2exp(x, rate = 1, shift = 0, log = FALSE)
p2exp(q, rate = 1, shift = 0, lower.tail = TRUE, log.p = FALSE)
q2exp(p, rate = 1, shift = 0, lower.tail = TRUE, log.p = FALSE)
r2exp(n, rate = 1, shift = 0)
}

\arguments{
  \item{x,q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{The number of observations.  If \code{length>1}, then the length is taken to be the number required.}
  \item{rate}{Vector of rates.}
  \item{shift}{Vector of shifts.}
  \item{log,log.p}{Logical vectors.  If \code{TRUE}, then probabilities are given as \code{log(p)}.}
  \item{lower.tail}{Logical vector.  If \code{TRUE}, then probabilities are \eqn{P[X\le x]}, else \eqn{P[X>x]}.}  
}
\details{
  If \code{rate} or \code{shift} are not specified, then they assume the default values of 1 and 0, respectively.
  
  The 2-parameter exponential distribution has density
  \deqn{f(x) = \frac{1}{\beta}e^{(x-\mu)/ \beta}}{%
        f(x) = exp(-(x-\mu)/\beta)/\beta,}
  where \eqn{x\ge\mu}, \eqn{\mu} is the shift parameter, and \eqn{\beta>0} is the scale parameter.
}
\value{
  \code{d2exp} gives the density, \code{p2exp} gives the distribution function, \code{q2exp} gives the quantile
  function, and \code{r2exp} generates random deviates.
}

\seealso{\code{\link{runif}} and \code{\link{.Random.seed}} about random number generation.}
\examples{
## Randomly generated data from the 2-parameter exponential 
## distribution.

set.seed(100)
x <- r2exp(n = 500, rate = 3, shift = -10)
hist(x, main = "Randomly Generated Data", prob = TRUE)

x.1 = sort(x)
y <- d2exp(x = x.1, rate = 3, shift = -10)
lines(x.1, y, col = 2, lwd = 2)

plot(x.1, p2exp(q = x.1, rate = 3, shift = -10), type = "l", 
     xlab = "x", ylab = "Cumulative Probabilities")

q2exp(p = 0.20, rate = 3, shift = -10, lower.tail = FALSE)
q2exp(p = 0.80, rate = 3, shift = -10)
}
\keyword{file}

