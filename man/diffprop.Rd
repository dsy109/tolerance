\name{DiffProp}
\alias{DiffProp}
\alias{ddiffprop}
\alias{pdiffprop}
\alias{qdiffprop}
\alias{rdiffprop}
\title{
Difference Between Two Proportions Distribution
}
\description{
Density (mass), distribution function, quantile function, and random generation for the difference between two proportions.  This is determined by taking the difference between two independent beta distributions. 
}
\usage{
ddiffprop(x, k1, k2, n1, n2, a1 = 0.5, a2 = 0.5,
          log = FALSE, ...)
pdiffprop(q, k1, k2, n1, n2, a1 = 0.5, a2 = 0.5,
          lower.tail = TRUE, log.p = FALSE, ...)
qdiffprop(p, k1, k2, n1, n2, a1 = 0.5, a2 = 0.5,
          lower.tail = TRUE, log.p = FALSE, ...)
rdiffprop(n, k1, k2, n1, n2, a1 = 0.5, a2 = 0.5)
}

\arguments{
    \item{x, q}{Vector of quantiles.}
    \item{p}{Vector of probabilities.}
    \item{n}{The number of observations.  If \code{length>1}, then the length is taken to be the number
    required.}
    \item{k1, k2}{The number of successes drawn from groups 1 and 2, respectively.}
    \item{n1, n2}{The sample sizes for groups 1 and 2, respectively.}
  \item{a1, a2}{The shift parameters for the beta distributions.  For the fiducial approach, we know that the lower and upper limits are set at \code{a1 = a2 = 0} and \code{a1 = a2 = 1}, respectively, for the true \code{p1} and \code{p2}.  While computations can be performed on real values outside the unit interval, a \code{warning} message will be returned if such values are specified. For practical purposes, the default value of 0.5 should be used for each parameter.}  
    \item{log, log.p}{Logical vectors.  If \code{TRUE}, then the probabilities are given as \code{log(p)}.}
    \item{lower.tail}{Logical vector.  If \code{TRUE}, then probabilities are \eqn{P[X\le x]}, else \eqn{P[X>x]}.}
    \item{...}{Additional arguments passed to the Appell \code{F1} function.}
}
\details{
The difference between two proportions distribution has a fairly complicated functional form.  Please see the article by Chen and Luo (2011), who corrected a typo in the article by Nadarajah and Kotz (2007), for the functional form of this distribution.
}
\references{
Chen, Y. and Luo, S. (2011), A Few Remarks on 'Statistical Distribution of the Difference of Two Proportions', \emph{Statistics in Medicine}, \bold{30}, 1913--1915. 

Nadarajah, S. and Kotz, S. (2007), Statistical Distribution of the Difference of Two Proportions, \emph{Statistics in Medicine}, \bold{26}, 3518--3523. 
}
\value{
\code{ddiffprop} gives the density (mass), \code{pdiffprop} gives the distribution function, \code{qdiffprop} gives the quantile function, and \code{rdiffprop} generates random deviates. 
}
\seealso{
\code{\link{runif}} and \code{\link{.Random.seed}} about random number generation.
}
\examples{
## Randomly generated data from the difference between
## two proportions distribution.

set.seed(100)
x <- rdiffprop(n = 100, k1 = 2, k2 = 10, n1 = 17, n2 = 13)
hist(x, main = "Randomly Generated Data", prob = TRUE)

x.1 <- sort(x)
y <- ddiffprop(x = x.1, k1 = 2, k2 = 10, n1 = 17, n2 = 13)
lines(x.1, y, col = 2, lwd = 2)

plot(x.1, pdiffprop(q = x.1, k1 = 2, k2 = 10, n1 = 17, 
     n2 = 13), type = "l", xlab = "x", 
     ylab = "Cumulative Probabilities")

qdiffprop(p = 0.20, k1 = 2, k2 = 10, n1 = 17, n2 = 13, 
          lower.tail = FALSE)
qdiffprop(p = 0.80, k1 = 2, k2 = 10, n1 = 17, n2 = 13)
}
\keyword{file}
