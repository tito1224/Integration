% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simpsons.R
\name{simpsons}
\alias{simpsons}
\title{Implement Simpson's Rule for Generic Function}
\usage{
simpsons(n, f, range)
}
\arguments{
\item{n}{The number of intervals used for approximation}

\item{f}{A function with a single argument x}

\item{range}{A vector containing the lower and upper endpoints of the range of integration}
}
\value{
Returns a single value which is the simpson approximation of the function over (a,b)
}
\description{
Implement Simpson's Rule for Generic Function
}
\examples{
simpsons(5,sqrt, c(1,3))

}
