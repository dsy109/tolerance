\name{K.factor.sim}
\title{Estimating K-factors for Simultaneous Tolerance Intervals Based on Normality}
\alias{K.factor.sim}
\usage{
K.factor.sim(n, l = NULL, alpha = 0.05, P = 0.99, side = 1, 
         method = c("EXACT", "BONF"), m = 50)
}
\description{
  Estimates k-factors for simultaneous tolerance intervals based on normality.
}

\arguments{
  \item{n}{If \code{method = "EXACT"}, this is the sample size of each of the \code{l} groups. If \code{method = "BONF"}, then \code{n} can be a vector of different sample sizes for the \code{l} groups.}
  \item{l}{The number of normal populations for which the k-factors will be constructed simultaneously.
  If \code{NULL}, then it is taken to be the length of \code{n}.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by the tolerance interval.}
  \item{side}{Whether a k-factor for a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2},
  respectively).}
  \item{method}{The method for calculating the k-factors.  \code{"EXACT"} is an exact method that can be used when all \code{l} groups have the same sample size. \code{"BONF"} is an approximate method using the Bonferroni inequality, which can be used when the \code{l} groups have different sample sizes.}
  \item{m}{The maximum number of subintervals to be used in the \code{integrate} function.  This is necessary only for \code{method = "EXACT"}.  The larger
  the number, the more accurate the solution.  Too low of a value can result in an error. A large value can also cause the function to be slow for \code{method = "EXACT"}.}
} \value{
  \code{K.factor} returns the k-factor for simultaneous tolerance intervals based on normality with the arguments specified above.
} \seealso{
  \code{\link{integrate}}, \code{\link{K.factor}}
}

\note{
For larger combinations of \code{n} and \code{l} when \code{side = 2} and \code{method = "EXACT"}, the calculation can be slow. For larger sample sizes when \code{method = "BONF"}, there may be some accuracy issues with the 1-sided calculation since it depends on the noncentral t-distribution.  
The code is primarily intended to be used for moderate values of the noncentrality parameter. It will not be highly accurate, especially in the tails, for large values.
See \code{\link{TDist}} for further details.

Thanks to Andrew Landgraf for providing the basic code for the \code{method = "EXACT"} procedure.
}

\references{
  Krishnamoorthy, K. and Mathew, T. (2009), \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}, Wiley.

  Mee, R. W. (1990), Simultaneous Tolerance Intervals for Normal Populations with Common Variance, \emph{Technometrics}, \bold{32}, 83-92.
} 

\examples{ 
## Reproducing part of Table B5 from Krishnamoorthy and 
## Mathew (2009).

n_sizes <- c(2:20, seq(30, 100, 10))
l_sizes <- 2:10
KM_table <- sapply(1:length(l_sizes), function(i)
                   sapply(1:length(n_sizes), function(j)
                   round(K.factor.sim(n = n_sizes[j], 
                   l = l_sizes[i], side=1, alpha = 0.1, 
                   P = 0.9),3)))
dimnames(KM_table) <- list(n = n_sizes, l = l_sizes)
KM_table

}

\keyword{file}


