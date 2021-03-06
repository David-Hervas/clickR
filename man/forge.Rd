% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive.R
\name{forge}
\alias{forge}
\title{Forge}
\usage{
forge(data, affixes, force.fixed = NULL, var.name = "time")
}
\arguments{
\item{data}{data.frame}

\item{affixes}{Affixes for repeated measures}

\item{force.fixed}{Variables with matching affix to be excluded}

\item{var.name}{Name for the new created variable (repetitions)}
}
\description{
Reshapes a data frame from wide to long format
}
\examples{
#Data frame in wide format
df1 <- data.frame(id = 1:4, age = c(20, 30, 30, 35), score1 = c(2,2,3,4),
                  score2 = c(2,1,3,1), score3 = c(1,1,0,1))
df1
#Data frame in long format
forge(df1, affixes= c("1", "2", "3"))

#Data frame in wide format with two repeated measured variables
df2 <- data.frame(df1, var1 = c(15, 20, 16, 19), var3 = c(12, 15, 15, 17))
df2
#Missing times are filled with NAs
forge(df2, affixes = c("1", "2", "3"))

#Use of parameter force.fixed
df3 <- df2[, -7]
df3
forge(df3, affixes=c("1", "2", "3"))
forge(df3, affixes=c("1", "2", "3"), force.fixed = c("var1"))
}
