\name{bintol.int}
\title{Binomial Tolerance Intervals}
\alias{bintol.int}
\usage{
bintol.int(x, n, m = NULL, alpha = 0.05, P = 0.99, side = 1, 
           method = c("LS", "WS", "AC", "JF", "CP", "AS", 
           "LO", "PR", "PO", "CL", "CC", "CWS"), 
           a1 = 0.5, a2 = 0.5)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for binomial random variables.  From a statistical quality control
  perspective, these limits use the proportion of defective (or acceptable) items in a sample to bound the number
  of defective (or acceptable) items in future productions of a specified quantity.
}

\arguments{
  \item{x}{The number of defective (or acceptable) units in the sample. Can be a vector of length \code{n}, in which case the sum of \code{x} is used.}
  \item{n}{The size of the random sample of units selected for inspection.}
  \item{m}{The quantity produced in future groups. If \code{m = NULL}, then the tolerance limits will be constructed assuming \code{n} for this quantity.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the defective (or acceptable) units in future samples of size \code{m} 
  to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for calculating the lower and upper confidence bounds, which are used in the calculation
  of the tolerance bounds.  The default method is \code{"LS"}, which is the large-sample method.  \code{"WS"} is Wilson's method, which is just the score confidence interval.  \code{"AC"} gives the Agresti-Coull method,
  which is also appropriate when the sample size is large.  \code{"JF"} is Jeffreys' method, which is a Bayesian approach
  to the estimation.  \code{"CP"} is the Clopper-Pearson (exact) method, which is based on beta percentiles and provides a more conservative interval.  \code{"AS"} is
  the arcsine method, which is appropriate when the sample proportion is not too close to 0 or 1.  \code{"LO"} is the logit
  method, which also is appropriate when the sample proportion is not too close to 0 or 1, but yields a more conservative interval.  \code{"PR"} uses a probit transformation and is accurate for large sample sizes.  \code{"PO"} is based on a Poisson parameterization, but it tends to be more erratic compared to the other methods.  \code{"CL"} is the complementary log transformation and also tends to perform well for large sample sizes.  \code{"CC"} gives a continuity-corrected version of the large-sample method.  \code{"CWS"} gives a continuity-corrected version of Wilson's method.
  More information on these methods can be found in the "References".}
  \item{a1}{This specifies the first shape hyperparameter when using Jeffreys' method.}
  \item{a2}{This specifies the second shape hyperparameter when using Jeffreys' method.}

} \value{
  \code{bintol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of defective (or acceptable) units in future samples of size \code{m}.}
  \item{p.hat}{The proportion of defective (or acceptable) units in the sample, calculated by \code{x/n}.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{Binomial}}, \code{\link{umatol.int}}
}

\references{
  Brown, L. D., Cai, T. T., and DasGupta, A. (2001), Interval Estimation for a Binomial Proportion, 
  \emph{Statistical Science}, \bold{16}, 101--133.

  Hahn, G. J. and Chandra, R. (1981), Tolerance Intervals for Poisson and Binomial Variables,
  \emph{Journal of Quality Technology}, \bold{13}, 100--110.

  Newcombe, R. G. (1998), Two-Sided Confidence Intervals for the Single Proportion: Comparison of Seven Methods, \emph{Statistics in Medicine}, \bold{17}, 857--872.
} 

\examples{ 
## 85\%/90\% 2-sided binomial tolerance intervals for a future 
## lot of 2500 when a sample of 230 were drawn from a lot of 
## 1000.  All methods but Jeffreys' method are compared
## below.

bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "LS")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "WS")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "AC")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "CP")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "AS")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "LO")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "PR")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "PO")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "CL")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "CC")
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 2, method = "CWS")
           
## Using Jeffreys' method to construct the 85\%/90\% 1-sided 
## binomial tolerance limits.  The first calculation assumes 
## a prior on the proportion of defects which places greater
## density on values near 0.  The second calculation assumes
## a prior on the proportion of defects which places greater
## density on values near 1.

bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 1, method = "JF", a1 = 2, a2 = 10)
bintol.int(x = 230, n = 1000, m = 2500, alpha = 0.15, P = 0.90,
           side = 1, method = "JF", a1 = 5, a2 = 1)


 }

\keyword{file}


