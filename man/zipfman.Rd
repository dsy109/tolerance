\name{ZipfMandelbrot}
\alias{ZipfMandelbrot}
\alias{dzipfman}
\alias{pzipfman}
\alias{qzipfman}
\alias{rzipfman}
\title{
Zipf-Mandelbrot Distributions
}
\description{
Density (mass), distribution function, quantile function, and random generation for the Zipf, Zipf-Mandelbrot, and zeta distributions. 
}
\usage{
dzipfman(x, s, b = NULL, N = NULL, log = FALSE)
pzipfman(q, s, b = NULL, N = NULL, lower.tail = TRUE, 
         log.p = FALSE)
qzipfman(p, s, b = NULL, N = NULL, lower.tail = TRUE, 
         log.p = FALSE)
rzipfman(n, s, b = NULL, N = NULL)
}

\arguments{
    \item{x, q}{Vector of quantiles.}
    \item{p}{Vector of probabilities.}
    \item{n}{The number of observations.  If \code{length>1}, then the length is taken to be the number
    required.}
    \item{s, b}{The shape parameters, both of which must be greater than 0.  \code{b} must be specified for Zipf-Mandelbrot distributions.}
    \item{N}{The number of categories, which must be integer-valued for Zipf and Zipf-Mandelbrot distributions. For a zeta distribution, \code{N = Inf} must be used.}
    \item{log, log.p}{Logical vectors.  If \code{TRUE}, then the probabilities are given as \code{log(p)}.}
    \item{lower.tail}{Logical vector.  If \code{TRUE}, then probabilities are \eqn{P[X\le x]}, else \eqn{P[X>x]}.}
}
\note{
These functions may be updated in a future version of the package so as to allow greater flexibility with the inputs.
} 
\details{
The Zipf-Mandelbrot distribution has mass
\deqn{p(x) = \frac{(x + b)^{-s}}{\sum_{i=1}^{N}(i + b)^{-s}},}{%
      p(x) = (x + b)^-s/\sum_{i=1}^{N}(i + b)^(-s),}	
where \eqn{x=1,\ldots,N}, \code{s,b>0} are shape parameters, and \code{N} is the number of distinct categories. The Zipf distribution is just a special case of the Zipf-Mandelbrot distribution where the second shape parameter \code{b=0}.  The zeta distribution has mass
\deqn{p(x) = \frac{x^{-s}}{\zeta(s)},}{%
      p(x) = x^-\lambda/\zeta(s),}	
where \eqn{x=1,2,\ldots}, \code{s>1} is the shape parameter, and \eqn{\zeta()} is the Riemann zeta function given by:
\deqn{\zeta(t) = \sum_{i=1}^{\infty}\frac{1}{i^{t}}<\infty.}{%
			\zeta(t) = \sum_{i=1}^\infty 1/i^t<\infty.}
Note that the zeta distribution is just a special case of the Zipf distribution where \code{s>1} and \code{N} goes to infinity.
}
\references{
Mandelbrot, B. B. (1965), Information Theory and Psycholinguistics. In B. B. Wolman and E. Nagel, editors. \emph{Scientific Psychology}, Basic Books.

Young, D. S. (2013), Approximate Tolerance Limits for Zipf-Mandelbrot Distributions, \emph{Physica A: Statistical Mechanics and its Applications}, \bold{392}, 1702--1711. 

Zipf, G. K. (1949), \emph{Human Behavior and the Principle of Least Effort}, Hafner.

Zornig, P. and Altmann, G. (1995), Unified Representation of Zipf Distributions, \emph{Computational Statistics and Data Analysis}, \bold{19}, 461--473. 
}
\value{
\code{dzipfman} gives the density (mass), \code{pzipfman} gives the distribution function, \code{qzipfman} gives the quantile function, and \code{rzipfman} generates random deviates for the specified distribution. 
}
\seealso{
\code{\link{runif}} and \code{\link{.Random.seed}} about random number generation.
}
\examples{
## Randomly generated data from the Zipf distribution.

set.seed(100)
x <- rzipfman(n = 150, s = 2, N = 100)
hist(x, main = "Randomly Generated Data", prob = TRUE)

x.1 <- sort(x)
y <- dzipfman(x = x.1, s = 2, N = 100)
lines(x.1, y, col = 2, lwd = 2)

plot(x.1, pzipfman(q = x.1, s = 2, N = 100), type = "l", 
     xlab = "x", ylab = "Cumulative Probabilities")

qzipfman(p = 0.20, s = 2, N = 100, lower.tail = FALSE)
qzipfman(p = 0.80, s = 2, N = 100)

## Randomly generated data from the Zipf-Mandelbrot distribution.

set.seed(100)
x <- rzipfman(n = 150, s = 2, b = 3, N = 100)
hist(x, main = "Randomly Generated Data", prob = TRUE)

x.1 <- sort(x)
y <- dzipfman(x = x.1, s = 2, b = 3, N = 100)
lines(x.1, y, col = 2, lwd = 2)

plot(x.1, pzipfman(q = x.1, s = 2, b = 3, N = 100), type = "l", 
     xlab = "x", ylab = "Cumulative Probabilities")

qzipfman(p = 0.20, s = 2, b = 3, N = 100, lower.tail = FALSE)
qzipfman(p = 0.80, s = 2, b = 3, N = 100)

## Randomly generated data from the zeta distribution.

set.seed(100)
x <- rzipfman(n = 100, s = 1.3, N = Inf)
hist(x, main = "Randomly Generated Data", prob = TRUE)

x.1 <- sort(x)
y <- dzipfman(x = x.1, s = 1.3, N = Inf)
lines(x.1, y, col = 2, lwd = 2)

plot(x.1, pzipfman(q = x.1, s = 1.3, N = Inf), type = "l", 
     xlab = "x", ylab = "Cumulative Probabilities")

qzipfman(p = 0.20, s = 1.3, lower.tail = FALSE, N = Inf)
qzipfman(p = 0.80, s = 1.3, N = Inf)
}
\keyword{file}
