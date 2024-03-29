% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_functions.R
\name{fix_all}
\alias{fix_all}
\title{fix_all}
\usage{
fix_all(x, select = 1:ncol(x), track = TRUE)
}
\arguments{
\item{x}{A data.frame}

\item{select}{Numeric vector with the positions (all by default) to be affected by the function}

\item{track}{Track changes?}
}
\description{
Tries to automatically fix all problems in the data.frame
}
