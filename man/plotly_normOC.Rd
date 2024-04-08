\name{plotly_normOC}
\title{Operating Characteristic (OC) Curves for K-Factors for Tolerance Intervals Based on Normality 
(a \code{plotly} version of \code{\link{norm.OC}})}
\alias{plotly_normOC}
\usage{
plotly_normOC(k = NULL, 
              alpha = NULL, 
              P = NULL, 
              n, 
              side = 1, 
              method = c("HE", "HE2", "WBE", "ELL", "KM", "EXACT", "OCT"), 
              m = 50,
              range.min = NULL,
              range.max = NULL,
              x.lab.size = NULL,
              y.lab.size = NULL,
              x.tick.size = NULL,
              y.tick.size = NULL,
              title = NULL,
              title.size = NULL,
              title.position.x = NULL,
              title.position.y = NULL,
              legend.size = NULL,
              x.cex = NULL,
              line.width = NULL,
              line.type = c("solid","dash","dot","dashdot"))
}
\description{
\code{plotly_normOC} is an updated function rooted in \code{norm.OC}.
}

\arguments{
  \item{k}{If wanting OC curves where the confidence level or content level is on the y-axis, then a single positive value of \code{k} must be specified.  This would be the target k-factor for the desired tolerance interval.  If \code{k = NULL}, then OC curves will be constructed where the k-factor value is found for given levels of \code{alpha}, \code{P}, and \code{n}.}
  \item{alpha}{The set of levels chosen such that \code{1-alpha} are confidence levels.  If wanting OC curves where the content level is being calculated, then each curve will correspond to a level in the set of \code{alpha}.  If a set of \code{P} values is specified, then OC curves will be constructed where the k-factor is found and each curve will correspond to each combination of \code{alpha} and \code{P}.  If \code{alpha = NULL}, then OC curves will be constructed to find the confidence level for given levels of \code{k}, \code{P}, and \code{n}.}
  \item{P}{The set of content levels to be considered.  If wanting OC curves where the confidence level is being calculated, then each curve will correspond to a level in the set of \code{P}.  If a set of \code{alpha} values is specified, then OC curves will be constructed where the k-factor is found and each curve will correspond to each combination of \code{alpha} and \code{P}.  If \code{P = NULL}, then OC curves will be constructed to find the content level for given levels of \code{k}, \code{alpha}, and \code{n}.}
  \item{n}{A sequence of sample sizes to consider.  This must be a vector of at least length 2 since all OC curves are constructed as functions of \code{n}.}
  \item{side}{Whether a 1-sided or 2-sided tolerance interval is required (determined by \code{side = 1} or \code{side = 2}, respectively).}
  \item{method}{The method for calculating the k-factors.  The k-factor for the 1-sided tolerance intervals
  is performed exactly and thus is the same for the chosen method.  \code{"HE"} is the
  Howe method and is often viewed as being extremely accurate, even for small sample sizes. \code{"HE2"} is a second method due to Howe, which performs similarly to the Weissberg-Beatty method, but is computationally simpler.  \code{"WBE"} is the 
  Weissberg-Beatty method (also called the Wald-Wolfowitz method), which performs similarly to the first Howe method for larger sample sizes. \code{"ELL"} is
  the Ellison correction to the Weissberg-Beatty method when \code{f} is appreciably larger than \code{n^2}. A warning
  message is displayed if \code{f} is not larger than \code{n^2}. \code{"KM"} is the Krishnamoorthy-Mathew approximation to the exact solution, which works well for larger sample sizes. \code{"EXACT"} computes the 
  k-factor exactly by finding the integral solution to the problem via the \code{integrate} function.  Note the computation time of this method is largely determined by \code{m}. \code{"OCT"} is the Owen approach 
  to compute the k-factor when controlling the tails so that there is not more than (1-P)/2  of the data in each tail of the distribution.}
  \item{m}{The maximum number of subintervals to be used in the \code{integrate} function, which is used for the underlying exact method for calculating the normal tolerance intervals.}
  \item{range.min}{The minimum value of the y-axis.}
  \item{range.max}{The maximum value of the y-axis.}
  \item{x.lab.size}{Size of label of the x-axis.}
  \item{y.lab.size}{Size of label of the y-axis.}
  \item{x.tick.size}{Size of tick marks on the x-axis.}
  \item{y.tick.size}{Sze of tick marks on the y-axis.}
  \item{title}{The main title on top of the plot.}
  \item{title.size}{Size of the title.}
  \item{title.position.x}{Horizontal position of the title.}
  \item{title.position.y}{Vertical position of the title.}
  \item{legend.size}{Size of the legend.}
  \item{x.cex}{Size of data points.}
  \item{line.width}{Width of lines connecting data points.}
  \item{line.type}{The type of lines connection data points. }
} 

\value{
  \code{\link{norm.OC}} returns a figure with the OC curves constructed using the specifications in the arguments.
} \seealso{
  \code{\link{K.factor}}, \code{\link{normtol.int}}, \code{\link{norm.OC}}
}


\references{
  Young, D. S. (2016), Normal Tolerance Interval Procedures in the tolerance Package, \emph{The R Journal}, \bold{8}, 200--212.
} 

\examples{ 
## The three types of OC-curves that can be constructed
## with the ggnorm.OC function.
 
plotly_normOC(k = 4, alpha = NULL, P = c(0.90, 0.95, 0.99), 
          n = 10:20, side = 1)

plotly_normOC(k = 4, alpha = c(0.01, 0.05, 0.10), P = NULL, 
          n = 10:20, side = 1)

plotly_normOC(k = NULL, P = c(0.90, 0.95, 0.99), 
          alpha=c(0.01,0.05,0.10), n = 10:20, side = 1) 
}

\keyword{file}
