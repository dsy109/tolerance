\name{zipftol.int}
\alias{zipftol.int}
\title{
Zipf-Mandelbrot Tolerance Intervals
}
\description{
Provides 1-sided or 2-sided tolerance intervals for data distributed according to Zipf, Zipf-Mandelbrot, and zeta distributions. 
}
\usage{
zipftol.int(x, m = NULL, N = NULL, alpha = 0.05, P = 0.99, 
            side = 1, s = 1, b = 1, dist = c("Zipf", 
            "Zipf-Man", "Zeta"), ties = FALSE, ...) 
}
\arguments{
    \item{x}{A vector of raw data or a table of counts which is distributed according to a Zipf, Zipf-Mandelbrot, or zeta distribution. Do not supply a vector of counts!}
	\item{m}{The number of observations in a future sample for which the tolerance limits will be calculated.  By default, \code{m = NULL} and, thus, \code{m} will be set equal to the original sample size.}
    \item{N}{The number of categories when \code{dist = "Zipf"} or \code{dist = "Zipf-Man"}.  This is not used when \code{dist = "Zeta"}.  If \code{N = NULL}, then \code{N} is estimated based on the number of categories observed in the data.}
    \item{alpha}{The level chosen such that 1-alpha is the confidence level.}
    \item{P}{The proportion of the population to be covered by this tolerance interval.}
    \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2}, respectively).}
    \item{s}{The initial value to estimate the shape parameter in the \code{zm.ll} function.}
    \item{b}{The initial value to estimate the second shape parameter in the \code{zm.ll} function when \code{dist = "Zipf-Man"}.}
    \item{dist}{Options are \code{dist = "Zipf"}, \code{dist = "Zipf-Man"}, or \code{dist = "Zeta"} if the data is distributed according to the Zipf, Zipf-Mandelbrot, or zeta distribution, respectively.}
    \item{ties}{How to handle if there are other categories with the same frequency as the category at the estimated tolerance limit.  The default is \code{FALSE}, which does no correction.  If \code{TRUE}, then the highest ranked (i.e., lowest number) of the tied categories is selected for the lower limit and the lowest ranked (i.e., highest number) of the tied categories is selected for the upper limit.}
    \item{...}{Additional arguments passed to the \code{zm.ll} function, which is used for maximum likelihood estimation.}
}
\note{
This function may be updated in a future version of the package so as to allow greater flexibility with the inputs.
} 
\details{
Zipf-Mandelbrot models are commonly used to model phenomena where the frequencies of categorical data are approximately inversely proportional to its rank in the frequency table. Zipf-Mandelbrot distributions are heavily right-skewed distributions with a (relatively) large mass placed on the first category.  For most practical applications, one will typically be interested in 1-sided upper bounds.
}
\references{
Mandelbrot, B. B. (1965), Information Theory and Psycholinguistics. In B. B. Wolman and E. Nagel, editors. \emph{Scientific Psychology}, Basic Books.

Young, D. S. (2013), Approximate Tolerance Limits for Zipf-Mandelbrot Distributions, \emph{Physica A: Statistical Mechanics and its Applications}, \bold{392}, 1702--1711. 

Zipf, G. K. (1949), \emph{Human Behavior and the Principle of Least Effort}, Hafner.

Zornig, P. and Altmann, G. (1995), Unified Representation of Zipf Distributions, \emph{Computational Statistics and Data Analysis}, \bold{19}, 461--473. 
}
\value{
\code{zipftol.int} returns a data frame with the following items:
		\item{\code{alpha}}{The specified significance level.} 
		\item{\code{P}}{The proportion of the population covered by this tolerance interval.} 
		\item{\code{s.hat}}{MLE for the shape parameter \code{s}.} 
		\item{\code{b.hat}}{MLE for the shape parameter \code{b} when \code{dist = "Zipf-Man"}.} 	
		\item{\code{1-sided.lower}}{The 1-sided lower tolerance bound. This is given only if \code{side = 1.}} 
		\item{\code{1-sided.upper}}{The 1-sided upper tolerance bound. This is given only if \code{side = 1.}} 
		\item{\code{2-sided.lower}}{The 2-sided lower tolerance bound. This is given only if \code{side = 2.}} 
		\item{\code{2-sided.upper}}{The 2-sided upper tolerance bound. This is given only if \code{side = 2.}} 				
}
\seealso{\code{\link{ZipfMandelbrot}}, \code{\link{zm.ll}}
}
\examples{
## 95\%/99\% 1-sided tolerance intervals for the Zipf, 
## Zipf-Mandelbrot, and zeta distributions. 

set.seed(100)

s <- 2
b <- 5
N <- 50

zipf.data <- rzipfman(n = 150, s = s, N = N)
zipfman.data <- rzipfman(n = 150, s = s, b = b, N = N)
zeta.data <- rzipfman(n = 150, s = s, N = Inf)

out.zipf <- zipftol.int(zipf.data, dist = "Zipf")
out.zipfman <- zipftol.int(zipfman.data, dist = "Zipf-Man")
out.zeta <- zipftol.int(zeta.data, N = Inf, dist = "Zeta")

out.zipf
out.zipfman
out.zeta
}
\keyword{file}
