\name{zm.ll}
\alias{zm.ll}
\title{
Maximum Likelihood Estimation for Zipf-Mandelbrot Models
}
\description{
Performs maximum likelihood estimation for the parameters of the Zipf, Zipf-Mandelbrot, and zeta distributions. 
}
\usage{
zm.ll(x, N = NULL, s = 1, b = 1, dist = c("Zipf", "Zipf-Man", 
      "Zeta"), ...) 
}
\arguments{
    \item{x}{A vector of raw data or a table of counts which is distributed according to a Zipf, Zipf-Mandelbrot, or zeta distribution. Do not supply a vector of counts!}
    \item{N}{The number of categories when \code{dist = "Zipf"} or \code{dist = "Zipf-Man"}.  This is not used when \code{dist = "Zeta"}.  If \code{N = NULL}, then \code{N} is estimated based on the number of categories observed in the data.}
    \item{s}{The initial value to estimate the shape parameter, which is set to 1 by default.  If a poor initial value is specified, then a \code{WARNING} message is returned.}
    \item{b}{The initial value to estimate the second shape parameter when \code{dist = "Zipf-Man"}, which is set to 1 by default. If a poor initial value is specified, then a \code{WARNING} message is returned.}
    \item{dist}{Options are \code{dist = "Zipf"}, \code{dist = "Zipf-Man"}, or \code{dist = "Zeta"} if the data is distributed according to the Zipf, Zipf-Mandelbrot, or zeta distribution, respectively.}
    \item{...}{Additional arguments passed to the \code{mle} function.}
}
\note{
This function may be updated in a future version of the package so as to allow greater flexibility with the inputs.
} 
\details{
Zipf-Mandelbrot models are commonly used to model phenomena where the frequencies of categorical data are approximately inversely proportional to its rank in the frequency table.
}
\references{
Mandelbrot, B. B. (1965), Information Theory and Psycholinguistics. In B. B. Wolman and E. Nagel, editors. \emph{Scientific Psychology}, Basic Books.

Zipf, G. K. (1949), \emph{Human Behavior and the Principle of Least Effort}, Hafner.

Zornig, P. and Altmann, G. (1995), Unified Representation of Zipf Distributions, \emph{Computational Statistics and Data Analysis}, \bold{19}, 461--473. 
}
\value{
See the help file for \code{mle} to see how the output is structured.				
}
\seealso{
\code{\link{mle}}, \code{\link{ZipfMandelbrot}}
}
\examples{
## Maximum likelihood estimation for randomly generated data
## from the Zipf, Zipf-Mandelbrot, and zeta distributions. 

set.seed(100)

s <- 2
b <- 5
N <- 50

zipf.data <- rzipfman(n = 500, s = s, N = N)
out.zipf <- zm.ll(zipf.data, N = N, dist = "Zipf")
stats4::coef(out.zipf)
stats4::vcov(out.zipf)

zipfman.data <- rzipfman(n = 500, s = s, b = b, N = N)
out.zipfman <- zm.ll(zipfman.data, N = N, dist = "Zipf-Man")
stats4::coef(out.zipfman)
diag(stats4::vcov(out.zipfman))

zeta.data <- rzipfman(n = 200, s = s, N = Inf)
out.zeta <- zm.ll(zeta.data, N = Inf, dist = "Zeta")
stats4::coef(out.zeta)
stats4::vcov(out.zeta)
}
\keyword{file}
