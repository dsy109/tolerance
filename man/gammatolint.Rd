\name{gamtol.int}
\title{Gamma (or Log-Gamma) Tolerance Intervals}
\alias{gamtol.int}
\usage{
gamtol.int(x, alpha = 0.05, P = 0.99, side = 1, 
           method = c("HE", "HE2", "WBE", "ELL", "KM", "EXACT", 
           "OCT"), m = 50, log.gamma = FALSE)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for data distributed according to either a gamma
  distribution or log-gamma distribution.
}

\arguments{
  \item{x}{A vector of data which is distributed according to either a gamma distribution or a log-gamma distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for calculating the k-factors.  The k-factor for the 1-sided tolerance intervals
  is performed exactly and thus is the same for the chosen method.  \code{"HE"} is the
  Howe method and is often viewed as being extremely accurate, even for small sample sizes. \code{"HE2"} is a second method due to Howe, which performs similarly to the Weissberg-Beatty method, but is computationally simpler.  \code{"WBE"} is the 
  Weissberg-Beatty method (also called the Wald-Wolfowitz method), which performs similarly to the first Howe method for larger sample sizes. \code{"ELL"} is
  the Ellison correction to the Weissberg-Beatty method when \code{f} is appreciably larger than \code{n^2}. A warning
  message is displayed if \code{f} is not larger than \code{n^2}. \code{"KM"} is the Krishnamoorthy-Mathew approximation to the exact solution, which works well for larger sample sizes. \code{"EXACT"} computes the 
  k-factor exactly by finding the integral solution to the problem via the \code{integrate} function.  Note the computation time of this method is largely determined by \code{m}. \code{"OCT"} is the Owen approach 
  to compute the k-factor when controlling the tails so that there is not more than (1-P)/2  of the data in each tail of the distribution.}
  \item{m}{The maximum number of subintervals to be used in the \code{integrate} function.  This is necessary only for \code{method = "EXACT"} and \code{method = "OCT"}.  The larger
  the number, the more accurate the solution.  Too low of a value can result in an error. A large value can also cause the function to be slow for \code{method = "EXACT"}.}
  \item{log.gamma}{If \code{TRUE}, then the data is considered to be from a log-gamma distribution, in which
  case the output gives tolerance intervals for the log-gamma distribution.  The default is \code{FALSE}.} 
} \value{
  \code{gamtol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{GammaDist}}, \code{\link{K.factor}}
}
\details{
Recall that if the random variable \eqn{X} is distributed according to a log-gamma distribution, then the random variable \eqn{Y = ln(X)} is
distributed according to a gamma distribution.
}

\references{
  Krishnamoorthy, K., Mathew, T., and Mukherjee, S. (2008), Normal-Based Methods for a Gamma Distribution:
  Prediction and Tolerance Intervals and Stress-Strength Reliability, \emph{Technometrics}, \bold{50}, 69--78.
} 

\examples{ 
## 99\%/99\% 1-sided gamma tolerance intervals for a sample
## of size 50. 

set.seed(100)
x <- rgamma(50, 0.30, scale = 2)
out <- gamtol.int(x = x, alpha = 0.01, P = 0.99, side = 1)
out

plottol(out, x, plot.type = "both", side = "upper", 
        x.lab = "Gamma Data")
}

\keyword{file}



