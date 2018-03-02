#' Checks if each value might be numeric
#'
#' @description Checks if each value from a vector might be numeric
#' @param x A vector
#' @return A logical vector
may.numeric <- function(x) suppressWarnings(!is.na(numeros(x)))

#' Extreme values from a numeric vector
#'
#' @description Returns the nth lowest and highest values from a vector
#' @param x A vector
#' @param n Number of extreme values to return
#' @param id ID column to reference the found extreme values
#' @return A matrix with the lowest and highest values from a vector
extreme_values <- function(x, n=5, id=NULL){
  h<-matrix(rep("", n*2), ncol=n)
  m<-matrix(sort(na.omit(numeros(x)))[c(1:n, (length(na.omit(numeros(x)))-(n-1)):length(na.omit(numeros(x))))], nrow=2, byrow=TRUE)
  if(!is.null(id)){
    h<- matrix(id[order(numeros(x))][c(1:n, (length(na.omit(numeros(x)))-(n-1)):length(na.omit(numeros(x))))], nrow=2, byrow=TRUE)
  }
  return(list(Values=t(m), IDs=t(h)))
}

#' Brute numeric coercion
#'
#' @description If possible, coerces values from a vector to numeric
#' @param x A vector
#' @return A numeric vector
numeros <- function(x) {
  suppressWarnings(as.numeric(gsub(paste(c(",", "\\.\\.", ",,", "\\.,", ",\\.", "\\."),
                                         collapse = "|"), ".", gsub("[A-Za-z]", "", iconv(gsub("^ *|(?<= ) | *$", "",
                                                                                               tolower(as.character(x)), perl = TRUE), to = "ASCII//TRANSLIT")))))
}

#' True TRUE
#'
#' @description Makes possible logical comparisons against NULL values
#' @param x A logical vector
#' @return A logical vector
ttrue<-function(x){
  x[is.null(x)]<-FALSE
  return(x)
}

#' Checks data quality of a variable
#'
#' @description Returns different data quality details of a numeric or categorical variable
#' @param x A variable from a data.frame
#' @param id ID column to reference the found extreme values
#' @param plot If the variable is numeric, should a boxplot be drawn?
#' @param numeric If set to TRUE, forces the variable to be considered numeric
#' @param n Number of extreme values to extract
#' @param output Format of the output. If TRUE, optimize for exporting as csv
#' @param ... further arguments passed to boxplot()
#' @return A list of a data.frame with information about data quality of the variable
#' @importFrom graphics boxplot dotchart
#' @importFrom stats setNames
#' @export
#' @examples
#' check_quality(airquality$Ozone)  #For one variable
#' lapply(airquality, check_quality)  #For a data.frame
#' lapply(airquality, check_quality, output=TRUE)  #For a data.frame, one row per variable
check_quality <- function(x, id=1:length(x), plot=TRUE, numeric=NULL, n=ifelse(is.numeric(x) | ttrue(numeric) | class(x) %in% "Date", 5, 2), output=FALSE, ...){
  call_n <- !is.null(as.list(match.call())$n)
  num <- numeric
  date <- class(x) %in% "Date"
  numbers <- sum(may.numeric(x))
  offending_values<-NA
  if(is.null(numeric)){
    if(numbers>(length(x)/10)) {
      num<-TRUE
      n<-max(c(n, 5*!call_n))} else num <- FALSE
  }
  if(num & !is.numeric(x)){
    warning("Numeric variable encoded as a factor. Use fix.numerics() to amend", call.=FALSE)
    off<-table(x)[is.na(numeros(names(table(x))))]
    offending_values<-paste(paste(names(off), " (", off, ")", sep=""), collapse="; ", sep="")
  }
  if(plot & num) boxplot(numeros(x), col="gray", outcol="darkred", pch=16, las=1, ylab=as.character(as.list(match.call())$x)[3], ...)
  if(plot & !num & !date) dotchart(sort(setNames(as.numeric(table(x)), names(table(x)))), pch=16)
  if(plot & date) plot(sort(x, na.last = TRUE), 1:length(x), pch=16, col=rgb(50, 50, 50, 100, maxColorValue = 255), las=1, xlab="Date", ylab="")
  Extremes_low<-NA
  Extremes_high<-NA
  Tabla2<-NA
  if(num){
    Extremes <- extreme_values(x, n, id)
  }
  if(!num & !date){
    Table <- data.frame(sort(table(x))[1:n])
    Tabla2<-paste(apply(data.frame(sort(table(x))[1:n]), 1, function(x) paste(x[1], " (", x[2], ")", sep="")), collapse="; ")
  }
  if(date){
    h<-matrix(rep("", n*2), ncol=2)
    m<-data.frame(Low=sort(na.omit(x))[1:n], high=sort(na.omit(x))[(length(na.omit(x))-(n-1)):length(na.omit(x))])
    if(!is.null(id)){
      h<- t(matrix(id[order(x)][c(1:n, (length(na.omit(x))-(n-1)):length(na.omit(x)))], nrow=2, byrow=TRUE))
    }
    Extremes <- list(Values=m, IDs=h)
  }
  if(num | date){
    Extremes_low <- gsub(";", "];", paste(paste(Extremes$Values[, 1], Extremes$IDs[, 1], sep=" ["), "; ", collapse="", sep=""))
    Extremes_high <- gsub(";", "];", paste(paste(Extremes$Values[, 2], Extremes$IDs[, 2], sep=" ["), "; ", collapse="", sep=""))
  }
  N.Categories <- length(table(x))
  Strings <- sum(grepl("[[:alpha:]]", x))
  NAs<-sum(is.na(x))
  whitespace<-sum(x %in% "" | x %in% " ", na.rm=TRUE)
  if(output){
    res<-data.frame(n=length(x), NAs=NAs, whitespace=whitespace, numbers=numbers, Strings=Strings,
                    N.Categories=N.Categories, Extremes_low=Extremes_low, Extremes_high=Extremes_high, Table=strtrim(Tabla2, 150),
                    Offenders=strtrim(offending_values, 150))
  } else{
    res<-list(Summary=data.frame(n=length(x), NAs=NAs, whitespace=whitespace, numbers=numbers, strings=Strings), Extremes=if(num | date) Extremes else cbind(Table, N.Categories=c(rep("", n-1), length(table(x)))), Offending=offending_values)
  }
  return(res)
}
