\name{ebind}
\alias{ebind}
\title{Empirical Binding Function}
\description{This function take a vector and adds two additional data points.}
\usage{
ebind (x)
}
\arguments{
\item{x}{A vector of data points.}
}
\examples{
x = rnorm (30, 4) ^ 2
x = ebind (x)
}
