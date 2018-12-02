\name{conditional}
\alias{epdfc}
\alias{ecdfc}
\alias{ecdfc.inverse}
\title{Nonparametric Conditional Probability Distributions}
\description{Empirical conditional probability density functions, empirical conditional cumulative distribution functions and empirical conditional quantile functions. Refer to the vignette for better examples.}
\usage{
epdfc (rv, conditions, x,
    restack.pdf=sbcpdf, rsp=0.5, bw,
    bind=TRUE, w=NA, is.string=FALSE, npoints=30)
ecdfc (rv, conditions, x,
    restack.pdf=sbcpdf, rsp=0.5, bw,
    bind=TRUE, w=NA, is.string=FALSE, npoints=30)
ecdfc.inverse (rv, conditions, x,
    restack.pdf=sbcpdf, rsp=0.5, bw,
    bind=TRUE, w=NA, is.string=FALSE, npoints=30)
}
\value{These functions return functions.}
\seealso{
\link{epdfuv}, \link{ecdfuv}, \link{ecdfuv.inverse},
\link{epdfmv}, \link{ecdfmv}
}
\arguments{
\item{rv}{A name of a random variable.}
\item{conditions}{A named vector of conditions.}
\item{x}{A vector of data points.}
\item{restack.pdf}{A PDF used for restacking.}
\item{rsp}{A restacking parameter. Refer to the vignette.}
\item{bw}{A bandwidth parameter. Refer to the vignette.}
\item{bind}{If true, add an extra two data points.}
\item{w}{A vector of weights.}
\item{is.string}{If true rv is expected to be a character type, otherwise, rv is expected to be an unquoted name of a variable.}
\item{npoints}{Number of synthetic data points.}
}
\examples{
#construct an empirical multivariate probability density function
#and then evaluate it
data (trees)
attach (trees)
epdfc.f = epdfc (Volume, c (Height=80), cbind (Height, Volume) )
epdfc.f (30)
}
