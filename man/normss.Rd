\name{norm.ss}
\title{Sample Size Determination for Normal Tolerance Intervals}
\alias{norm.ss}
\usage{
norm.ss(x = NULL, alpha = 0.05, P = 0.99, delta = NULL,
        P.prime = NULL, side = 1, m = 50, spec = c(NA, NA),
        hyper.par = list(mu.0 = NULL, sig2.0 = NULL, 
        m.0 = NULL, n.0 = NULL), method = c("DIR", 
        "FW", "YGZO"))
}
\description{
Provides minimum sample sizes for a future sample size when constructing normal tolerance intervals.  Various strategies are available for determining the sample size, including strategies that incorporate known specification limits.
}

\arguments{
  \item{x}{A vector of current data that is distributed according to a normal distribution.  This is only required for \code{method = "YGZO"}.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{delta}{The precision measure for the future tolerance interval as specified under the Faulkenberry-Weeks method.}
  \item{P.prime}{The proportion of the population (greater than \code{P}) such that the tolerance interval of interest will only exceed \code{P.prime} by the probability given by \code{delta}.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2}, respectively).}
  \item{m}{The maximum number of subintervals to be used in the \code{integrate} function, which is used for the underlying exact method for calculating the normal tolerance intervals.}
  \item{spec}{A vector of length 2 given known specification limits.  These are required when \code{method = "DIR"} or \code{method = "YGZO"}.  By default, the values are \code{NA}.  The two elements of the vector are for the lower and upper specification limits, respectively.  If \code{side = 1}, then only one of the specification limits must be specified.  If \code{side = 2}, then both specification limits must be specified.}
  \item{hyper.par}{Necessary parameter values for the different methods.  If \code{method = "DIR"} or \code{method = "YGZO"}, then \code{mu.0} and \code{sig2.0} must be specified, which correspond to the assumed population mean and variance of the underlying normal distribution, which further pertains to the historical data for \code{method = "YGZO"}.  If \code{method = "YGZO"} and the sample size is to be determined using Bayesian normal tolerance intervals, then this is a required list consisting of the hyperparameters for the conjugate prior -- the hyperparameters for the mean (\code{mu.0 and n.0}) and the hyperparameters for the variance (\code{sig2.0} and \code{m.0}).}
  \item{method}{The method for performing the sample size determination.   \code{"DIR"} is the direct method (intended as a simple calculation for planning purposes) where the mean and standard deviation are taken as truth and the sample size is determined with respect to the given specification limits.  \code{"FW"} is for the traditional Faulkenberry-Weeks approach for sample size determination. \code{"YGZO"} is for the Young-Gordon-Zhu-Olin approach, which incorporates historical data and specification limits for determining the value of \code{delta} and/or \code{P.prime} in the Faulkenberry-Weeks approach.  Note that for \code{"YGZO"}, at least one of \code{delta} and \code{P.prime} must be \code{NULL}.}
} \value{
  \code{norm.ss} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{delta}{The user-specified or calculated precision measure.  Not returned if \code{method = "DIR"}.}
  \item{P.prime}{The user-specified or calculated closeness measure.  Not returned if \code{method = "DIR"}.}
  \item{n}{The minimum sample size determined using the conditions specified for this function.}
} \seealso{
  \code{\link{bayesnormtol.int}}, \code{\link{Normal}}, \code{\link{normtol.int}}
}


\references{
  Faulkenberry, G. D. and Weeks, D. L. (1968), Sample Size Determination for Tolerance Limits, \emph{Technometrics}, \bold{10}, 343--348.

  Young, D. S., Gordon, C. M., Zhu, S., and Olin, B. D. (2016), Sample Size Determination Strategies for Normal Tolerance Intervals Using Historical Data, \emph{Quality Engineering}, \bold{28}, 337--351.
} 

\examples{ 
## Sample size determination for 95\%/95\% 2-sided normal 
## tolerance intervals using the direct method.
 
set.seed(100)
norm.ss(alpha = 0.05, P = 0.95, side = 2, spec = c(-3, 3), 
        method = "DIR", hyper.par = list(mu.0 = 0, 
        sig2.0 = 1))

}

\keyword{file}




