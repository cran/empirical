\name{returned_functions}
\alias{epdfuv.f}
\alias{ecdfuv.f}
\alias{ecdfuv.f.inverse}
\alias{epdfmv.f}
\alias{ecdfmv.f}
\title{Returned Functions}
\description{Functions returned by functions. Note that you can name your functions whatever you like. (In the vignette I've named them f, F and F.inv). Also note that you don't call these functions, rather call epdfuv(), ecdfuv(), etc and then call the functions that are returned.}
\seealso{\link{epdfuv}, \link{ecdfuv}, \link{ecdfuv.inverse},
\link{epdfmv}, \link{ecdfmv},
\link{epdfc}, \link{ecdfc}, \link{ecdfc.inverse}
}
\usage{
epdfuv.f (x)
ecdfuv.f (x)
ecdfuv.f.inverse (y)
epdfmv.f (x)
ecdfmv.f (x)
}
\arguments{
\item{x}{A vector of x values.}
\item{y}{A vector of y values.}
}
\examples{
#construct an empirical quantile function and then evaluate it
#(compute the 0.25 quantile)
x = rnorm (30, 4) ^ 2
ecdfuv.f.inverse = ecdfuv.inverse (x)
ecdfuv.f.inverse (0.25)
}
