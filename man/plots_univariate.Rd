\name{plots_univariate}
\alias{plot.epdfuv}
\alias{plot.ecdfuv}
\alias{plot.ecdfuv.inverse}
\alias{lines.epdfuv}
\alias{lines.ecdfuv}
\alias{lines.ecdfuv.inverse}
\title{Plots Univariate}
\description{Plots of univariate probability distributions.}
\seealso{\link{epdfuv}, \link{ecdfuv}, \link{ecdfuv.inverse}}
\usage{
\method{plot}{epdfuv}(x, plot.points=FALSE, \dots)
\method{plot}{ecdfuv}(x, plot.points=FALSE, \dots)
\method{plot}{ecdfuv.inverse}(x, plot.points=FALSE, \dots)
\method{lines}{epdfuv}(x, \dots)
\method{lines}{ecdfuv}(x, \dots)
\method{lines}{ecdfuv.inverse}(x, \dots)
}
\arguments{
\item{x}{An epdfuv, ecdfuv, ecdfuv.inverse, epdfc, ecdfc or ecdfc.inverse object.}
\item{plot.points}{Plot the data points.}
\item{\dots}{Other arguments.}
}
\examples{
#plot an empirical univariate cumulative distribution function
data (trees)
attach (trees)
ecdfuv.f = ecdfuv (Height)
plot (ecdfuv.f)
}
