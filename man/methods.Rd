\name{methods}
\alias{print.euvpdf}
\alias{print.euvcdf}
\alias{print.euvcdf.inverse}
\alias{plot.euvpdf}
\alias{plot.euvcdf}
\alias{plot.euvcdf.inverse}
\title{Methods}
\description{Empirical probability distribution methods.}
\seealso{\link{euvpdf}, \link{euvcdf}, \link{euvcdf.inverse}}
\usage{
\method{print}{euvpdf}(x, \dots)
\method{print}{euvcdf}(x, \dots)
\method{print}{euvcdf.inverse}(x, \dots)
\method{plot}{euvpdf}(x, points=FALSE, \dots)
\\method{plot}{euvcdf}(x, points=FALSE, \dots)
\method{plot}{euvcdf.inverse}(x, \dots)
}
\arguments{
\item{x}{An epdf, ecdf or ecdf.inverse object.}
\item{points}{Plot the data points.}
\item{\dots}{Other arguments.}
}
\examples{
#print an empirical cumulative distribution function
x = rnorm (30, 4) ^ 2
euvcdf.f = euvcdf (x, TRUE)
euvcdf.f
}
