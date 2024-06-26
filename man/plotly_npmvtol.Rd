\name{plotly_npmvtol}
\title{plotting Nonparametric Multivaraite Hyperrectangular Tolerance Region}
\alias{plotly_npmvtol}
\usage{
plotly_npmvtol(tol.out, 
               x, 
               var.names = NULL, 
               title = NULL,
               x.col = "#4298B5",
               x.cex = 6,
               x.shape = "dot",
               outlier.col = "#A6192E",
               outlier.cex = 8,
               outlier.shape = "triangle-up",
               tol.col = "#D1DDE6",
               tol.opacity = 0.4,
               x.lab.size = 12,
               x.tick.size = 12,
               y.lab.size = 12,
               y.tick.size = 12,
               title.position.x = 0.5,
               title.position.y = 0.98,
               title.size = 12,
               show.bound = TRUE, 
               bound.type = c("dash", "dot", "solid", "longdash", 
                              "dashdot", "longdashdot"), 
               bound.col = "#000000",
               bound.lwd = 1
               )
}
\description{
\code{plotly_npmvtol} is plotting function for nonparametric multivaraite hyperrectangular tolerance region. The function takes the outcome of \code{npmvtol.region} as an input and provides visualzation for hypperrectangular tolerance regions between two variables.
}

\arguments{
  \item{tol.out}{Output from \code{npmvtol.region} for multivariate data.}
  \item{x}{Data frame for different variables. Columns of \code{x} represent for different variables.}
  \item{var.names}{Labels of variable names. The dimension of \code{var.names} needs to be consistent with column dimension of \code{x}.}
  \item{title}{The main title on top of the plot. The length of \code{title} can be either 1 or multiple. If only 1 title is specified, all plots share the same title. If multiple titles are specified, number of titles needs to be consistent with the number of combinations of variables. For example, if the data has 4 variables, either 1 or 6 (choose 2 out of 4) titles need to be specified.}
  \item{x.col}{Color of original data points, excluding outliers.}
  \item{x.cex}{Size of original data points, excluding outliers.}
  \item{x.shape}{Shape of original data points, excluding outliers.}
  \item{outlier.col}{Color of outliers.}
  \item{outlier.cex}{Size of outliers.}
  \item{outlier.shape}{Shape of outliers.}
  \item{tol.col}{Color of tolerance region.}
  \item{tol.opacity}{Opacity of tolerance region.}
  \item{x.lab.size}{Size of label of the x-axis.}
  \item{x.tick.size}{Size of tick marks on the x-axis.}
  \item{y.lab.size}{Size of label of the y-axis.}
  \item{y.tick.size}{Size of tick marks on the y-axis.}
  \item{title.position.x}{Horizontal position of the title.}
  \item{title.position.y}{Vertical position of the title.}
  \item{title.size}{Size of the title.}
  \item{show.bound}{Logical indicating to show rectanglular boundaries.  Default is \code{TRUE}.}
  \item{bound.type}{Line type of the rectangle boundaries.}
  \item{bound.col}{Color of the rectangle boundaries.}
  \item{bound.lwd}{Width of the rectangle boundaries.}
} 

\value{
  \code{plotly_npmvtol} returns figures of hypperectangular tolerance regions between two random variable generated by \code{npmvtol.region}.
} \seealso{
  \code{\link{npmvtol.region}}
}


\references{
  Young, D. S., & Mathew, T. (2020), Nonparametric Hyperrectangular Tolerance and Prediction Regions for Setting Multivariate Reference Regions in Laboratory Medicine. \emph{Statistical Methods in Medical Research}, \bold{29}, 3569--3585.
} 

\examples{ 
library(plotly)

mdepth <- function(pts, x){
  mahalanobis(pts, center = rep(0, 3),
              cov = diag(1, 3))
}

set.seed(100)
x <- cbind(X1=rnorm(300), X2=rnorm(300), X3=rnorm(300))
out <-npmvtol.region(x = x, alpha = 0.10, P = 0.90, depth.fn = mdepth,
                     type = "semispace", semi.order = list(lower = 2, 
                                                           center = 3, upper = 1))

gg.out <- plotly_npmvtol(tol.out = out , x = x)
}

