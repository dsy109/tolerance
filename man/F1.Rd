\name{F1}
\title{Appell's F1 Hypergeometric Function}
\alias{F1}
\usage{
F1(a, b, b.prime, c, x, y, ...)
}
\description{
  The Appell function of the first kind, which is a two variable extension of the hypergeometric distribution.
}

\arguments{
  \item{a, b, b.prime, c}{Appropriate parameters for this function.}
  \item{x, y}{The inputted values to evaluate this function such that each is less than 1 in absolute value.}
  \item{...}{Additional arguments passed to the \code{integrate} function.}
} \value{
  \code{F1} returns the simple integral result for the Appell function of the first kind with the arguments specified above.
} \seealso{
  \code{\link{DiffProp}}, \code{\link{integrate}}
}

\note{
This function is solved by using a simple integral representation for real numbers.  While all four of the Appell functions can be extended to the complex plane, this is not an option for this code.
}

\references{
  Bailey, W. N. (1935), \emph{Generalised Hypergeometric Series}, Cambridge University Press.  
} 

\examples{ 
## Sample calculation.

F1(a = 3, b = 4, b.prime = 5, c = 13, x = 0.2, y = 0.4)
}

\keyword{file}


