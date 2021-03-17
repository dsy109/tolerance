\name{bayesnormtol.int}
\title{Bayesian Normal Tolerance Intervals}
\alias{bayesnormtol.int}
\usage{
bayesnormtol.int(x = NULL, norm.stats = list(x.bar = NA, 
                 s = NA, n = NA), alpha = 0.05, P = 0.99, 
                 side = 1, method = c("HE", "HE2", "WBE", 
                 "ELL", "KM", "EXACT", "OCT"), m = 50,
                 hyper.par = list(mu.0 = NULL, 
                 sig2.0 = NULL, m.0 = NULL, n.0 = NULL))
}
\description{
Provides 1-sided or 2-sided Bayesian tolerance intervals under the conjugate prior for data distributed according to a normal distribution.
}

\arguments{
  \item{x}{A vector of data which is distributed according to a normal distribution.}
  \item{norm.stats}{An optional list of statistics that can be provided in-lieu of the full dataset.  If provided, the user must specify all three components: the sample mean (\code{x.bar}), the sample standard deviation (\code{s}), and the sample size (\code{n}).}
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
  \item{hyper.par}{A list consisting of the hyperparameters for the conjugate prior: the hyperparameters for the mean (\code{mu.0} and \code{n.0}) and the hyperparameters for the variance (\code{sig2.0} and \code{m.0}).} 
} \value{
  \code{bayesnormtol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{x.bar}{The sample mean.}
  \item{1-sided.lower}{The 1-sided lower Bayesian tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper Bayesian tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower Bayesian tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper Bayesian tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{Normal}}, \code{\link{normtol.int}}, \code{\link{K.factor}}
}

\details{
Note that if one considers the non-informative prior distribution, then the Bayesian tolerance intervals are the same as the classical solution, which can be obtained by using \code{\link{normtol.int}}.
}


\references{
  Aitchison, J. (1964), Bayesian Tolerance Regions, \emph{Journal of the
  Royal Statistical Society, Series B}, \bold{26}, 161--175.

  Guttman, I. (1970), \emph{Statistical Tolerance Regions: Classical and Bayesian},
  Charles Griffin and Company.

  Young, D. S., Gordon, C. M., Zhu, S., and Olin, B. D. (2016), Sample Size Determination Strategies for Normal Tolerance Intervals Using Historical Data, \emph{Quality Engineering}, \bold{28}, 337--351.
} 

\examples{ 
## 95\%/85\% 1-sided Bayesian normal tolerance limits for 
## a sample of size 100. 

set.seed(100)
x <- rnorm(100)
out <- bayesnormtol.int(x = x, alpha = 0.05, P = 0.85, 
                        side = 1, method = "EXACT", 
                        hyper.par = list(mu.0 = 0, 
                        sig2.0 = 1, n.0 = 10, m.0 = 10))
out

plottol(out, x, plot.type = "both", side = "upper", 
        x.lab = "Normal Data")
}

\keyword{file}




