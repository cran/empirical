\name{plots_bivariate}
\alias{plot.epdfmv}
\alias{plot.ecdfmv}
\title{Plots Bivariate}
\description{Plots of multivariate probability distributions (with m=2).}
\usage{
\method{plot}{epdfmv}(x, use.plot3d=FALSE,
    xlab="x1", ylab="x2", npoints=30, ..., all=FALSE)
\method{plot}{ecdfmv}(x, use.plot3d=FALSE,
    xlab="x1", ylab="x2", npoints=30, ...)
}
\seealso{\link{epdfmv}, \link{ecdfmv} }
\arguments{
\item{x}{An epdfmv or ecdfmv object.}
\item{use.plot3d}{If true, use a 3d plot, otherwise, use a 2d plot.}
\item{xlab}{The x label.}
\item{ylab}{The y label.}
\item{npoints}{The number of grid points in each direction.}
\item{\dots}{Other arguments.}
\item{all}{If true, plot all combinations.}
}
\examples{
#plot an empirical multivariate cumulative distribution function
#(with m=2)
data (trees)
attach (trees)
ecdfmv.f = ecdfmv (cbind (Height, Volume) )
plot (ecdfmv.f, TRUE)
}
