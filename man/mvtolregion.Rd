\name{mvtol.region}
\title{Multivariate Normal Tolerance Regions}
\alias{mvtol.region}
\usage{
mvtol.region(x, alpha = 0.05, P = 0.99, B = 1000, M = 1000,
             method = c("KM", "AM", "GM", "HM", "MHM", "V11", 
             "HM.V11", "MC")) 
}
\description{
  Determines the appropriate tolerance factor for computing multivariate normal tolerance regions based on Monte Carlo methods or other approximations.
}

\arguments{
  \item{x}{An \code{n}x\code{p} matrix of data assumed to be drawn from a \code{p}-dimensional multivariate normal
  distribution.  \code{n} pertains to the sample size.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.  A vector of \code{alpha} values
  may be specified.}
  \item{P}{The proportion of the population to be covered by this tolerance region.  A vector of \code{P} values may be
  specified.}
  \item{B}{The number of iterations used for the Monte Carlo algorithms (i.e., when \code{method = "KM"} or \code{"MC"}), which determines the tolerance factor.  The number of
  iterations should be at least as large as the default value of 1000.}
  \item{M}{The number of iterations used for the inner loop of the Monte Carlo algorithm specified through \code{method = "MC"}.  The number of
  iterations should be at least as large as the default value of 1000.  Note that this is not required for \code{method = "KM"} since that algorithm handles the eigenvalues
  differently in the estimation of the tolerance factor.}
  \item{method}{The method for estimating the tolerance factors. \code{"KM"} is the Krishnamoorthy-Mondal method, which is the method implemented in previous versions of the \code{tolerance}
  package.  It is one of the more accurate methods available.  \code{"AM"} is an approximation method based on the arithmetic mean.  \code{"GM"} is an approximation method based on the geometric
  mean.  \code{"HM"} is an approximation method based on the harmonic mean.  \code{"MHM"} is a modified approach based on the harmonic mean.  \code{"V11"} is a method that utilizes a certain
  partitioning of a Wishart random matrix for deriving an approximate tolerance factor.  \code{"HM.V11"} is a hybrid method of the \code{"HM"} and \code{"V11"} methods.  \code{"MC"} is a simple Monte
  Carlo approach to estimating the tolerance factor, which is computationally expensive as the values of \code{B} and \code{M} increase.}
} \value{
  \code{mvtol.region} returns a matrix where the rows pertain to each confidence level \code{1-alpha} specified and the columns
  pertain to each proportion level \code{P} specified.
}

\references{
  Krishnamoorthy, K. and Mathew, T. (1999), Comparison of Approximation Methods for Computing Tolerance Factors for a Multivariate Normal Population, 
  \emph{Technometrics}, \bold{41}, 234--249.

  Krishnamoorthy, K. and Mondal, S. (2006), Improved Tolerance Factors for Multivariate Normal Distributions, 
  \emph{Communications in Statistics - Simulation and Computation}, \bold{35}, 461--478.
} 

\details{
  All of the methods are outlined in the references that we provided.  In practice, we recommend using the Krishnamoorthy-Mondal approach.  A basic sketch of how the Krishnamoorthy-Mondal algorithm works is as follows:
  
  (1) Generate independent chi-square random variables and Wishart random matrices.
  
  (2) Compute the eigenvalues of the randomly generated Wishart matrices.
  
  (3) Iterate the above steps to generate a set of \code{B} sample values such that the \code{100(1-alpha)}-th percentile is an approximate tolerance factor.
}


\examples{ 
## 90\%/90\% bivariate normal tolerance region. 

set.seed(100)
x1 <- rnorm(100, 0, 0.2)
x2 <- rnorm(100, 0, 0.5)
x <- cbind(x1, x2)

out1 <- mvtol.region(x = x, alpha = 0.10, P = 0.90, B = 1000,
                     method = "KM")
out1
plottol(out1, x)

## 90\%/90\% trivariate normal tolerance region. 

set.seed(100)
x1 <- rnorm(100, 0, 0.2)
x2 <- rnorm(100, 0, 0.5)
x3 <- rnorm(100, 5, 1)
x <- cbind(x1, x2, x3)
mvtol.region(x = x, alpha = c(0.10, 0.05, 0.01), 
             P = c(0.90, 0.95, 0.99), B = 1000, method = "KM") 

out2 <- mvtol.region(x = x, alpha = 0.10, P = 0.90, B = 1000, 
                     method = "KM")
out2
plottol(out2, x)
}

\keyword{file}




