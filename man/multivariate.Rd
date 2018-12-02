\name{multivariate}
\alias{epdfmv}
\alias{ecdfmv}
\title{Nonparametric Multivariate Probability Distributions}
\description{Empirical multivariate probability density functions and empirical multivariate cumulative distribution functions. Refer to the vignette for better examples.}
\usage{
epdfmv (x,
    restack.pdf=sbcpdf, restack.cdf=sbccdf,
    rsp=0.5, bw, bind=TRUE, w=NA)
ecdfmv (x,
    restack.pdf=sbcpdf, restack.cdf=sbccdf,
    rsp=0.5, bw, bind=TRUE, w=NA)
}
\value{These functions return functions.}
\seealso{
\link{epdfuv}, \link{ecdfuv}, \link{ecdfuv.inverse},
\link{epdfc}, \link{ecdfc}, \link{ecdfc.inverse},
\link{epdfmv.f}, \link{ecdfmv.f}
}
\arguments{
\item{x}{A vector of data points.}
\item{restack.pdf}{A PDF used for restacking. ECDFs ignore this.}
\item{restack.cdf}{A CDF used for restacking. EPDFs only use this if plotting bivariate models with all=TRUE.}
\item{rsp}{A restacking parameter. Refer to the vignette.}
\item{bw}{A bandwidth parameter. Refer to the vignette.}
\item{bind}{If true, add an extra two data points.}
\item{w}{A vector of weights.}
}
\examples{
#construct an empirical multivariate probability density function
#and then evaluate it
data (trees)
attach (trees)
epdfmv.f = epdfmv (cbind (Height, Volume) )
epdfmv.f (c (80, 30) )
}
