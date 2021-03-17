\name{umatol.int}
\title{Uniformly Most Accurate Upper Tolerance Limits for Certain Discrete Distributions}
\alias{umatol.int}
\usage{
umatol.int(x, n = NULL, dist = c("Bin", "NegBin", "Pois"), N, 
           alpha = 0.05, P = 0.99)
}
\description{
  Provides uniformly most accurate upper tolerance limits for the binomial, negative binomial, and Poisson distributions. 
}

\arguments{
  \item{x}{A vector of data which is distributed according to one of the binomial, negative binomial, or Poisson distributions.
  If the length of \code{x} is 1, then it is assumed that this number is the sum of iid values from the assumed distribution.}
  \item{n}{The sample size of the data.  If \code{null}, then \code{n} is calculated as the length of \code{x}.}
  \item{dist}{The distribution for the data given by \code{x}.  The options are \code{"Bin"} for the binomial distribution,
  \code{"NegBin"} for the negative binomial distribution, and \code{"Pois"} for the Poisson distribution.}
  \item{N}{Must be specified for the binomial and negative binomial distributions.  If \code{dist = "Bin"}, then \code{N}
  is the number of Bernoulli trials and must be a positive integer.  If \code{dist = "NegBin"}, then \code{N} is the total number
  of successful trials (or dispersion parameter) and must be strictly positive.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
} \value{
  \code{umatol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{p.hat}{The maximum likelihood estimate for the probability of success in each trial; reported if \code{dist = "Bin"}.}
  \item{nu.hat}{The maximum likelihood estimate for the probability of success in each trial; reported if \code{dist = "NegBin"}.}
  \item{lambda.hat}{The maximum likelihood estimate for the rate of success; reported if \code{dist = "Pois"}.}
  \item{1-sided.upper}{The 1-sided upper tolerance limit.}
} 

\references{
  Zacks, S. (1970), Uniformly Most Accurate Tolerance Limits for Monotone Likelihood Ratio Families of Discrete Distributions,
  \emph{Journal of the American Statistical Association}, \bold{65}, 307--316.
} 

\seealso{
  \code{\link{Binomial}}, \code{\link{NegBinomial}}, \code{\link{Poisson}}
}

\examples{ 
## Examples from Zacks (1970).

umatol.int(25, n = 4, dist = "Bin", N = 10, alpha = 0.10, 
           P = 0.95)
umatol.int(13, n = 10, dist = "NegBin", N = 2, alpha = 0.10,
           P = 0.95)
umatol.int(37, n = 10, dist = "Pois", alpha = 0.10, P = 0.95)
}

\keyword{file}





