\name{poislind.ll}
\alias{poislind.ll}
\title{
Maximum Likelihood Estimation for the Discrete Poisson-Lindley Distribution
}
\description{
Performs maximum likelihood estimation for the parameter of the Poisson-Lindley distribution. 
}
\usage{
poislind.ll(x, theta = NULL, ...) 
}
\arguments{
    \item{x}{A vector of raw data which is distributed according to a Poisson-Lindley distribution.}
    \item{theta}{Optional starting value for the parameter.  If \code{NULL}, then the method of moments estimator is used.}
    \item{...}{Additional arguments passed to the \code{mle} function.}
}
\details{
The discrete Poisson-Lindley distribution is a compound distribution that, potentially, provides a better fit for count data relative to the traditional Poisson and negative binomial distributions.
}
\references{
Ghitany, M. E. and Al-Mutairi, D. K. (2009), Estimation Methods for the Discrete Poisson-Lindley Distribution, 
\emph{Journal of Statistical Computation and Simulation}, \bold{79}, 1--9.

Sankaran, M. (1970), The Discrete Poisson-Lindley Distribution, \emph{Biometrics}, \bold{26}, 145--149. 
}
\value{
See the help file for \code{mle} to see how the output is structured.				
}
\seealso{
\code{\link{mle}}, \code{\link{PoissonLindley}}
}
\examples{
## Maximum likelihood estimation for randomly generated data
## from the Poisson-Lindley distribution. 

set.seed(100)

pl.data <- rpoislind(n = 500, theta = 0.5)
out.pl <- poislind.ll(pl.data)
stats4::coef(out.pl)
stats4::vcov(out.pl)
}
\keyword{file}
