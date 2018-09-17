\name{returned.functions}
\alias{euvpdf.f}
\alias{euvcdf.f}
\alias{euvcdf.f.inverse}
\title{Returned Functions}
\description{Returned functions. Note that you can name your functions whatever you like. (In the vignette I've named them f, F and F.inverse). Also note that you don't call these functions, rather call euvpdf(), euvcdf() or euvcdf.inverse and then call the functions that are returned.}
\seealso{\link{euvpdf}, \link{euvcdf}, \link{euvcdf.inverse}}
\usage{
euvpdf.f (x)
euvcdf.f (x)
euvcdf.f.inverse (y)
}
\arguments{
\item{x}{A vector a x values}
\item{y}{A vector of y values.}
}
\examples{
#construct and evaluate an empirical cumulative distribution function
x = rnorm (30, 4) ^ 2
euvcdf.f = euvcdf (x, TRUE)
euvcdf.f (0)
}
