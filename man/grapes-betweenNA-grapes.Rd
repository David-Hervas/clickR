% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_quality.R
\name{\%betweenNA\%}
\alias{\%betweenNA\%}
\title{between operator & not NA}
\usage{
x \%betweenNA\% y
}
\arguments{
\item{x}{Vector for the left side of the operator}

\item{y}{A vector of length two with the lower and upper values of the interval}
}
\value{
A logical vector of the same length as x
}
\description{
Operator equivalent to x >= lower.value & x <= upper.value & !is.na(x)
}
