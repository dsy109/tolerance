\name{anovatol.int}
\title{Tolerance Intervals for ANOVA}
\alias{anovatol.int}
\usage{
anovatol.int(lm.out, data, alpha = 0.05, P = 0.99, side = 1,
             method = c("HE", "HE2", "WBE", "ELL", "KM", 
             "EXACT", "OCT"), m = 50)
}
\description{
  Tolerance intervals for each factor level in a balanced (or nearly-balanced) ANOVA.
}

\arguments{
  \item{lm.out}{An object of class \code{lm} (i.e., the results from the linear model fitting routine such that
  the \code{anova} function can act upon).}
  \item{data}{A data frame consisting of the data fitted in \code{lm.out}.  Note that \code{data} must have one
  column for each main effect (i.e., factor) that is analyzed in \code{lm.out} and that these columns must be of
  class \code{factor}.}
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
} \value{
  \code{anovatol.int} returns a list where each element is a data frame corresponding to each main effect (i.e.,
  factor) tested in the ANOVA and the rows of each data frame are the levels of that factor.  The columns of each data
  frame report the following:
  \item{mean}{The mean for that factor level.}
  \item{n}{The effective sample size for that factor level.}
  \item{k}{The k-factor for constructing the respective factor level's tolerance interval.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.  This is given only if \code{side = 1}.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.  This is given only if \code{side = 1}.}
  \item{2-sided.lower}{The 2-sided lower tolerance bound.  This is given only if \code{side = 2}.}
  \item{2-sided.upper}{The 2-sided upper tolerance bound.  This is given only if \code{side = 2}.}
} \seealso{
  \code{\link{K.factor}}, \code{\link{normtol.int}}, \code{\link{lm}}, \code{\link{anova}}
} 


\references{
  Howe, W. G. (1969), Two-Sided Tolerance Limits for Normal Populations - Some Improvements, \emph{Journal of the
  American Statistical Association}, \bold{64}, 610--620.

  Weissberg, A. and Beatty, G. (1969), Tables of Tolerance Limit Factors for Normal Distributions, \emph{Technometrics},
  \bold{2}, 483--500.
} 

\examples{ 
## 90\%/95\% 2-sided tolerance intervals for a 2-way ANOVA 
## using the "warpbreaks" data.

attach(warpbreaks)

lm.out <- lm(breaks ~ wool + tension)
out <- anovatol.int(lm.out, data = warpbreaks, alpha = 0.10,
                    P = 0.95, side = 2, method = "HE")
out

plottol(out, x = warpbreaks)
}

\keyword{file}


