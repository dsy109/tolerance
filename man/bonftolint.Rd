\name{bonftol.int}
\title{Approximate 2-Sided Tolerance Intervals that Control the Tails Using Bonferroni's Inequality}
\alias{bonftol.int}
\usage{
bonftol.int(fn, P1 = 0.005, P2 = 0.005, alpha = 0.05, ...)
}
\description{
  This function allows the user to control what proportion of the population is to be in the tails of the given distribution for
  a 2-sided tolerance interval.  The result is a conservative approximation based on Bonferroni's inequality.
}

\arguments{
  \item{fn}{The function name for the 2-sided tolerance interval to be calculated.}
  \item{P1}{The proportion of the population not covered in the lower tail of the distribution.}
  \item{P2}{The proportion of the population not covered in the upper tail of the distribution.}
  \item{alpha}{The level chosen such that \code{1-alpha} is the confidence level.}
  \item{...}{Additional arguments passed to \code{fn}, including the data.  All arguments that would be specified in \code{fn} must
  also be specified here.}

} \value{
  The results for the 2-sided tolerance interval procedure are reported.  See the corresponding help file for \code{fn} about
  specific output.  Note that the (minimum) proportion of the population to be covered by this interval is \code{1 - (P1 + P2)}.
} \note{
  This function can be used with any 2-sided tolerance interval function, including the regression tolerance interval functions.
}

\references{
  Jensen, W. A. (2009), Approximations of Tolerance Intervals for Normally Distributed Data, \emph{Quality and Reliability
  Engineering International}, \bold{25}, 571--580.

  Patel, J. K. (1986), Tolerance Intervals - A Review, \emph{Communications in Statistics - Theory and Methodology},
  \bold{15}, 2719--2762.
} 

\examples{ 
## 95\%/97\% tolerance interval for normally distributed
## data controlling 1\% of the data is in the lower tail
## and 2\% of the data in the upper tail.

set.seed(100)
x <- rnorm(100, 0, 0.2)
bonftol.int(normtol.int, x = x, P1 = 0.01, P2 = 0.02,
            alpha = 0.05, method = "HE")

}

\keyword{file}


