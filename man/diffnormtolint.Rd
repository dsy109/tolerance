\name{diffnormtol.int}
\title{1-Sided Tolerance Limits for the Distribution of the Difference Between Two Independent Normal Random Variables}
\alias{diffnormtol.int}
\usage{
diffnormtol.int(x1, x2, var.ratio = NULL, alpha = 0.05, 
                P = 0.99, method = c("HALL", "GK", "RG"))
}
\description{
  Provides 1-sided tolerance limits for the difference between two independent normal random variables.  If the ratio of the variances is known,
  then an exact calculation is performed.  Otherwise, approximation methods are implemented.
}

\arguments{
  \item{x1}{A vector of sample data which is distributed according to a normal distribution (sample 1).}
  \item{x2}{Another vector of sample data which is distributed according to a normal distribution (sample 2).  It can be of a different sample size than
  the sample specified by \code{x1}.}
  \item{var.ratio}{A specified, known value of the variance ratio (i.e., the ratio of the variance for population 1 to the variance of population 2).
  If \code{NULL}, then the variance ratio is estimated according to one of the three methods specified in the \code{method} argument.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{P}{The proportion of the population to be covered by the tolerance limits.}
  \item{method}{The method for estimating the variance ratio.  This only needs to be specified in the case when
  \code{var.ratio} is not \code{NULL}.  \code{"HALL"} is Hall's method, which takes a bias-corrected version of the ratio between the sample variance for sample 1 to
  the sample variance for sample 2.  \code{"GK"} is the Guo-Krishnamoorthy method, which first calculates a bias-corrected version of the ratio between the sample variance for sample 2
  to the sample variance for sample 1.  The resulting limit is then compared to the limit from Hall's method and the most conservative limit is chosen.  \code{"RG"} is
  the Reiser-Guttman method, which is a biased version of the variance ratio that is calculated by taking the sample variance for sample 1 to the sample variance for sample 2.  
  Typically, Hall's method or the Guo-Krishnamoorthy method are preferred to the Reiser-Guttman method. }
} \value{
  \code{diffnormtol.int} returns a data frame with items:
  \item{alpha}{The specified significance level.}
  \item{P}{The proportion of the population covered by this tolerance interval.}
  \item{diff.bar}{The difference between the sample means.}
  \item{1-sided.lower}{The 1-sided lower tolerance bound.}
  \item{1-sided.upper}{The 1-sided upper tolerance bound.}
} \seealso{
  \code{\link{Normal}}, \code{\link{K.factor}}, \code{\link{normtol.int}}
}

\details{
Satterthwaite's approximation for the degrees of freedom is used when the variance ratio is unknown.
}

\note{
Unlike other tolerance interval functions, the output from \code{diffnormtol.int} cannot be passed to \code{plottol}.
}


\references{
  Guo, H. and Krishnamoorthy, K. (2004), New Approximate Inferential Methods for the Reliability Parameter in a Stress-Strength Model: The
  Normal Case, \emph{Communications in Statistics - Theory and Methods}, \bold{33}, 1715--1731.

  Hall, I. J. (1984), Approximate One-Sided Tolerance Limits for the Difference or Sum of Two Independent Normal Variates, \emph{Journal of Quality
  Technology}, \bold{16}, 15--19.

  Krishnamoorthy, K. and Mathew, T. (2009), \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}, Wiley.

  Reiser, B. J. and Guttman, I. (1986), Statistical Inference for Pr(Y < X): The Normal Case, \emph{Technometrics},
  \bold{28}, 253--257.
} 

\examples{ 
## 90\%/99\% tolerance limits for the difference between two
## simulated normal data sets.  This data is taken from
## Krishnamoorthy and Mathew (2009).  Note that there is a
## calculational error in their example, which yields different
## results with the output below. 

x1 <- c(10.166, 5.889, 8.258, 7.303, 8.757)
x2 <- c(-0.204, 2.578, 1.182, 1.892, 0.786, -0.517, 1.156,
        0.980, 0.323, 0.437, 0.397, 0.050, 0.812, 0.720)

diffnormtol.int(x1, x2, alpha = 0.10, P = 0.99, method = "HALL")
diffnormtol.int(x1, x2, alpha = 0.10, P = 0.99, method = "GK")
diffnormtol.int(x1, x2, alpha = 0.10, P = 0.99, method = "RG")
diffnormtol.int(x1, x2, var.ratio = 3.8, alpha = 0.10, P = 0.99)
}

\keyword{file}




