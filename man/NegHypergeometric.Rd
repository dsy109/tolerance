\name{NegHypergeometric}
\alias{NegHypergeometric}
\alias{dnhyper}
\alias{pnhyper}
\alias{qnhyper}
\alias{rnhyper}

\title{The Negative Hypergeometric Distribution}
\description{
  Density, distribution function, quantile function, and random generation for the negative hypergeometric distribution. 
}
\usage{
dnhyper(x, m, n, k, log = FALSE)
pnhyper(q, m, n, k, lower.tail = TRUE, log.p = FALSE)
qnhyper(p, m, n, k, lower.tail = TRUE, log.p = FALSE)
rnhyper(nn, m, n, k)
}

\arguments{
  \item{x,q}{Vector of quantiles representing the number of trials until \code{k} successes have occurred (e.g., until \code{k} white balls have been drawn from an urn without replacement).}
  \item{m}{The number of successes in the population (e.g., the number of white balls in the urn).}
  \item{n}{The population size (e.g., the total number of balls in the urn).}
  \item{k}{The number of successes (e.g., white balls) to achieve with the sample.}
  \item{p}{Vector of probabilities, which must be between 0 and 1.}
  \item{nn}{The number of observations.  If \code{length>1}, then the length is taken to be the number required.}
  \item{log,log.p}{Logical vectors.  If \code{TRUE}, then probabilities are given as \code{log(p)}.}
  \item{lower.tail}{Logical vector.  If \code{TRUE}, then probabilities are \eqn{P[X\le x]}, else \eqn{P[X>x]}.}  
}
\details{
  A negative hypergeometric distribution (sometimes called the inverse hypergeometric distribution) models the total number of trials until \code{k} successes occur.  Compare this to the negative binomial distribution, which models the number of failures that occur until a specified number of successes has been reached.  The negative hypergeometric distribution has density
  \deqn{p(x) = \frac{{x-1 \choose k-1}{n-x \choose m-k}}{{n \choose m}}}{%
        p(x) = choose(x-1, k-1)choose(n-x, m-k) / choose(n, m)}
  for \eqn{x=k,k+1,...,n-m+k}.
}
\value{
  \code{dnhyper} gives the density, \code{pnhyper} gives the distribution function, \code{qnhyper} gives the quantile
  function, and \code{rnhyper} generates random deviates.

  Invalid arguments will return value \code{NaN}, with a warning.
}
\references{
  Wilks, S. S. (1963), \emph{Mathematical Statistics}, Wiley.
} 
\seealso{
\code{\link{runif}} and \code{\link{.Random.seed}} about random number generation.
}
\examples{
## Randomly generated data from the negative hypergeometric 
## distribution.

set.seed(100)
x <- rnhyper(nn = 1000, m = 15, n = 40, k = 10)
hist(x, main = "Randomly Generated Data", prob = TRUE)

x.1 = sort(x)
y <- dnhyper(x = x.1, m = 15, n = 40, k = 10)
lines(x.1, y, col = 2, lwd = 2)

plot(x.1, pnhyper(q = x.1, m = 15, n = 40, k = 10),             
     type = "l", xlab = "x", ylab = "Cumulative Probabilities")

qnhyper(p = 0.20, m = 15, n = 40, k = 10, lower.tail = FALSE)
qnhyper(p = 0.80, m = 15, n = 40, k = 10)
}
\keyword{file}

