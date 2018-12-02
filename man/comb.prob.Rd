\name{comb.prob}
\alias{comb.prob}
\title{Compute Probabilities}
\description{Compute probabilities combinatorially.}
\usage{
comb.prob (F, a, b)
}
\seealso{\link{ecdfmv} }
\arguments{
\item{F}{An ecdfmv object.}
\item{a}{Either a vector or a matrix. Each element (or column) gives the lower limits.}
\item{b}{Either a vector or a matrix. Each element (or column) gives the upper limits.}
}
\examples{
data (trees)
attach (trees)
ecdfmv.f = ecdfmv (cbind (Height, Volume) )
a = c (0, 0)
b = c (80, 30)
comb.prob (ecdfmv.f, a, b)
}
