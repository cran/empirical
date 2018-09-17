\name{empirical}
\alias{euvpdf}
\alias{euvcdf}
\alias{euvcdf.inverse}
\title{Empirical Probability Distributions}
\description{Empirical cumulative distribution functions and inverses. Refer to the vignette for better examples.}
\usage{
euvpdf (x, smoothness, randomize=TRUE)
euvcdf (x, continuous=FALSE, randomize=TRUE)
euvcdf.inverse (x, continuous=FALSE)
}
\value{These functions return functions.}
\seealso{\link{euvpdf.f}, \link{euvcdf.f}, \link{euvcdf.f.inverse}}
\arguments{
\item{x}{A vector of data points.}
\item{continuous}{If true, a continuous function. Otherwise, a step function.}
\item{smoothness}{A smoothness paramter. Refer to the vignette.}
\item{randomize}{If a continuous function, true and there a duplicated values, add a small amount of random variation. If a continuous function, false and there a duplicated values, generate an error.}
}
\examples{
#construct a continuous empirical cumulative distribution function.
x = rnorm (30, 4) ^ 2
euvcdf.f = euvcdf (x, TRUE)
}
