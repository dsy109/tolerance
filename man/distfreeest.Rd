\name{distfree.est}
\title{Estimating Various Quantities for Distribution-Free Tolerance Intervals}
\alias{distfree.est}
\usage{
distfree.est(n = NULL, alpha = NULL, P = NULL, side = 1)
}
\description{
  When providing two of the three quantities \code{n}, \code{alpha}, and \code{P}, this function solves for the 
  third quantity in the context of distribution-free tolerance intervals.
}

\arguments{
  \item{n}{The necessary sample size to cover a proportion \code{P} of the population with
  confidence \code{1-alpha}.  Can be a vector.}
  \item{alpha}{1 minus the confidence level attained when it is desired to cover a proportion \code{P}
  of the population and a sample size \code{n} is provided.  Can be a vector.}
  \item{P}{The proportion of the population to be covered with confidence \code{1-alpha} when a sample size \code{n}
  is provided.  Can be a vector.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is assumed (determined by \code{side = 1} or \code{side = 2},
  respectively).}

} \value{
  When providing two of the three quantities \code{n}, \code{alpha}, and \code{P}, \code{distfree.est} returns the
  third quantity.  If more than one value of a certain quantity is specified, then a table will be returned.
} \seealso{
  \code{\link{nptol.int}}
}

\references{
  Natrella, M. G. (1963), \emph{Experimental Statistics: National Bureau of Standards - Handbook No. 91},
  United States Government Printing Office, Washington, D.C. 
} 

\examples{ 
## Solving for 1 minus the confidence level.

distfree.est(n = 59, P = 0.95, side = 1)

## Solving for the sample size.

distfree.est(alpha = 0.05, P = 0.95, side = 1)

## Solving for the proportion of the population to cover.

distfree.est(n = 59, alpha = 0.05, side = 1)

## Solving for sample sizes for many tolerance specifications.

distfree.est(alpha = seq(0.01, 0.05, 0.01), 
             P = seq(0.80, 0.99, 0.01), side = 2)


}

\keyword{file}


