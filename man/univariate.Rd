\name{univariate}
\alias{epdfuv}
\alias{ecdfuv}
\alias{ecdfuv.inverse}
\title{Nonparametric Univariate Probability Distributions}
\description{Empirical univariate probability density functions, empirical univariate cumulative distribution functions and empirical univariate quantile functions. Refer to the vignette for better examples.}
\usage{
epdfuv (x, derandomize=TRUE, preserve="mean",
    drp, nhood, bind=TRUE, randomize=TRUE, w=NA)
ecdfuv (x, derandomize=TRUE, preserve="mean",
    drp, nhood, bind=TRUE, randomize=TRUE, w=NA)
ecdfuv.inverse (x, derandomize=TRUE, preserve="mean",
    drp, nhood, bind=TRUE, randomize=TRUE, w=NA)
}
\value{These functions return functions.}
\seealso{
\link{epdfmv}, \link{ecdfmv},
\link{epdfc}, \link{ecdfc}, \link{ecdfc.inverse},
\link{epdfuv.f}, \link{ecdfuv.f}, \link{ecdfuv.f.inverse}
}
\arguments{
\item{x}{A vector of data points.}
\item{derandomize}{If true, smooth the data points.}
\item{preserve}{Either "mean" or "range". If derandomize and mean (the default), preserve the mean and variance. If derandomize and range, preserve the range.}
\item{drp}{A smoothness (derandomization) parameter. Refer to the vignette.}
\item{nhood}{A neighborhood size parameter. Refer to the vignette.}
\item{bind}{If true, add an extra two data points.}
\item{randomize}{If there a duplicated values, add a small amount of random variation.}
\item{w}{A vector of weights.}
}
\examples{
#construct an empirical univariate probability density function
#and then evaluate it
data (trees)
attach (trees)
epdfuv.f = epdfuv (Height)
epdfuv.f (80)
}
