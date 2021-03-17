\name{exttol.int}
\title{Weibull (or Extreme-Value) Tolerance Intervals}
\alias{exttol.int}
\usage{
exttol.int(x, alpha = 0.05, P = 0.99, side = 1,
           dist = c("Weibull", "Gumbel"), ext = c("min", "max"), 
           NR.delta = 1e-8)
}
\description{
  Provides 1-sided or 2-sided tolerance intervals for data distributed according to either a Weibull distribution or
  extreme-value (also called Gumbel) distributions.
}

\arguments{
  \item{x}{A vector of data which is distributed according to either a Weibull distribution or an extreme-value
  distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by this tolerance interval.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{dist}{Select either \code{dist = "Weibull"} or \code{dist = "Gumbel"} if the data is distributed according
  to the Weibull or extreme-value distribution, respectively.}
  \item{ext}{If \code{dist = "Gumbel"}, then select which extreme is to be modeled for the Gumbel distribution.  The
  Gumbel distribution for the minimum (i.e., \code{ext = "min"}) corresponds to a left-skewed distribution and the
  Gumbel distribution for the maximum (i.e., \code{ext = "max"}) corresponds to a right-skewed distribution}
  \item{NR.delta}{The stopping criterion used for the Newton-Raphson algorithm when finding the maximum likelihood
  estimates of the Weibull or extreme-value distribution.} 
} \value{
  \code{exttol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{shape.1}{MLE for the shape parameter if \code{dist = "Weibull"} or for the location parameter if
  \code{dist = "Gumbel"}.}
  \item{shape.2}{MLE for the scale parameter if \code{dist = "Weibull"} or \code{dist = "Gumbel"}.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{Weibull}}
}

\details{
Recall that the relationship between the Weibull distribution and the extreme-value distribution for the minimum is that if the
random variable \eqn{X} is distributed according to a Weibull distribution, then the random variable \eqn{Y = ln(X)} is
distributed according to an extreme-value distribution for the minimum.

If \code{dist = "Weibull"}, then the natural logarithm of the data are taken so that a Newton-Raphson algorithm can
be employed to find the MLEs of the extreme-value distribution for the minimum and then the data and MLEs are transformed back appropriately.
No transformation is performed if \code{dist = "Gumbel"}.  The Newton-Raphson algorithm is initialized by the method of moments
estimators for the parameters.
}


\references{
  Bain, L. J. and Engelhardt, M. (1981), Simple Approximate Distributional Results for Confidence and Tolerance Limits
  for the Weibull Distribution Based on Maximum Likelihood Estimators, \emph{Technometrics}, \bold{23}, 15--20.

  Coles, S. (2001), \emph{An Introduction to Statistical Modeling of Extreme Values}, Springer.
} 

\examples{ 
## 85\%/90\% 1-sided Weibull tolerance intervals for a sample
## of size 150. 

set.seed(100)
x <- rweibull(150, 3, 75)
out <- exttol.int(x = x, alpha = 0.15, P = 0.90, side = 1,
                  dist = "Weibull")
out

plottol(out, x, plot.type = "both", side = "lower", 
        x.lab = "Weibull Data")
}

\keyword{file}



