\name{K.factor}
\title{Estimating K-factors for Tolerance Intervals Based on Normality}
\alias{K.factor}
\usage{
K.factor(n, f = NULL, alpha = 0.05, P = 0.99, side = 1, 
         method = c("HE", "HE2", "WBE", "ELL", "KM", "EXACT", 
         "OCT"), m = 50)
}
\description{
  Estimates k-factors for tolerance intervals based on normality.
}

\arguments{
  \item{n}{The (effective) sample size.}
  \item{f}{The number of degrees of freedom associated with calculating the estimate of the population standard deviation.
  If \code{NULL}, then \code{f} is taken to be \code{n-1}.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by the tolerance interval.}
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
} \value{
  \code{K.factor} returns the k-factor for tolerance intervals based on normality with the arguments specified above.
} \seealso{
  \code{\link{integrate}}, \code{\link{K.table}}, \code{\link{normtol.int}}, \code{\link{TDist}}
}

\note{
For larger sample sizes, there may be some accuracy issues with the 1-sided calculation since it depends on the noncentral t-distribution.  
The code is primarily intended to be used for moderate values of the noncentrality parameter. It will not be highly accurate, especially in the tails, for large values.
See \code{\link{TDist}} for further details.
}

\references{
  Ellison, B. E. (1964), On Two-Sided Tolerance Intervals for a Normal Distribution, \emph{Annals of Mathematical
  Statistics}, \bold{35}, 762--772.
  
  Howe, W. G. (1969), Two-Sided Tolerance Limits for Normal Populations - Some Improvements, \emph{Journal of the
  American Statistical Association}, \bold{64}, 610--620.

  Krishnamoorthy, K. and Mathew, T. (2009), \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}, Wiley.
  
  Odeh, R. E. and Owen, D. B. (1980), \emph{Tables for Normal Tolerance Limits, Sampling Plans, and Screening}, Marcel-Dekker.

  Owen, D. B. (1964), Controls of Percentages in Both Tails of the Normal Distribution, \emph{Technometrics}, \bold{6}, 377-387.

  Wald, A. and Wolfowitz, J. (1946), Tolerance Limits for a Normal Distribution, \emph{Annals of the Mathematical Statistics},
  \bold{17}, 208--215.  

  Weissberg, A. and Beatty, G. (1969), Tables of Tolerance Limit Factors for Normal Distributions, \emph{Technometrics},
  \bold{2}, 483--500.
} 

\examples{ 
## Showing the effect of the Howe, Weissberg-Beatty, 
## and exact estimation methods as the sample size increases.

K.factor(10, P = 0.95, side = 2, method = "HE")
K.factor(10, P = 0.95, side = 2, method = "WBE")
K.factor(10, P = 0.95, side = 2, method = "EXACT", m = 50)

K.factor(100, P = 0.95, side = 2, method = "HE")
K.factor(100, P = 0.95, side = 2, method = "WBE")
K.factor(100, P = 0.95, side = 2, method = "EXACT", m = 50)

}

\keyword{file}


