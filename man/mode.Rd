\name{mode}
\alias{emode}
\title{Empirical Mode}
\description{Compute the mode of data using the empirical probability density function. Note that there are some problems using all=TRUE or warning=TRUE.}
\usage{
emode (f, include.boundaries=TRUE, all=FALSE, warning=FALSE)
}
\seealso{\link{epdfuv}, \link{epdfc} }
\arguments{
\item{f}{An epdfuv or epdfc object.}
\item{include.boundaries}{Include the min and max data values.}
\item{all}{If true, return all modal points, otherwise, return the max modal point only.}
\item{warning}{If true generate a warning if there are no or multiple modal points.}
}
\examples{
#compute the empirical mode
x = rnorm (30, 4) ^ 2
epdfuv.f = epdfuv (x)
emode (epdfuv.f)
}
