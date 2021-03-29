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
  m<-matrix(sort(na.omit(x))[c(1:n, (length(na.omit(x))-(n-1)):length(na.omit(x)))], nrow=2, byrow=TRUE)
  if(!is.null(id)){
    h<- matrix(id[order(x)][c(1:n, (length(na.omit(x))-(n-1)):length(na.omit(x)))], nrow=2, byrow=TRUE)
  }
  return(list(Values=setNames(data.frame(t(m)), c("Low", "High")), IDs=setNames(data.frame(t(h)), c("Low", "High"))))
}

#' Brute numeric coercion
#'
#' @description If possible, coerces values from a vector to numeric
#' @param x A vector
#' @return A numeric vector
numeros <- function(x){
  suppressWarnings(
    as.numeric(
      gsub(
        paste(c(",", "\\.\\.", ",,", "\\.,", ",\\.", "\\."),
              collapse = "|"),
        ".",
        gsub(
          "[A-Za-z]",
          "",
          iconv(
            gsub(
              "^ *|(?<= ) | *$",
              "",
              gsub("\\$|\u20ac|\u00A3",
                   "",
                   tolower(as.character(x))),
              perl = TRUE),
            to = "ASCII//TRANSLIT")))))
}

#' True TRUE
#'
#' @description Makes possible vectorized logical comparisons against NULL and NA values
#' @param x A logical vector
#' @return A logical vector
#' @export
ttrue <- function(x){
  x[is.na(x)] <- FALSE
  if(length(x) == 0L) x <- FALSE
  x
}

#' greater & NA
#'
#' @description '>' operator where NA values return FALSE
#' @param x Vector for the left side of the operator
#' @param y A Scalar or vector of the same length as x for the right side of the operator
#' @return A logical vector of the same length as x
#' @export
`%>NA%` <- function(x, y){
  ttrue(x > y)
}

#' less & NA
#'
#' @description '<' operator where NA values return FALSE
#' @param x Vector for the left side of the operator
#' @param y A Scalar or vector of the same length as x for the right side of the operator
#' @return A logical vector of the same length as x
#' @export
`%<NA%` <- function(x, y){
  ttrue(x < y)
}

#' geq & not NA
#'
#' @description '>=' operator where NA values return FALSE
#' @param x Vector for the left side of the operator
#' @param y A Scalar or vector of the same length as x for the right side of the operator
#' @return A logical vector of the same length as x
#' @export
`%>=NA%` <- function(x, y){
  ttrue(x >= y)
}

#' leq & not NA
#'
#' @description '<=' operator where NA values return FALSE
#' @param x Vector for the left side of the operator
#' @param y A Scalar or vector of the same length as x for the right side of the operator
#' @return A logical vector of the same length as x
#' @export
`%<=NA%` <- function(x, y){
  ttrue(x <= y)
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
    warning("Numeric variable encoded as a factor. Use fix_numerics() to amend", call.=FALSE)
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

#' Explores global environment workspace
#'
#' @description Returns information regarding the different objects in global environment
#' @param table If TRUE a table with the frequencies of each type of object is given
#' @return A list of object names by class or a table with frequencies if table = TRUE
#' @export
#' @examples
#' df1 <- data.frame(x=rnorm(10), y=rnorm(10, 1, 2))
#' df2 <- data.frame(x=rnorm(20), y=rnorm(20, 1, 2))
#' workspace(table=TRUE)  #Frequency table of the different object classes
#' workspace()  #All objects in the global object separated by class
workspace <- function(table=FALSE) {
  list_obj <- split(objects(envir=.GlobalEnv), sapply(objects(envir=.GlobalEnv), function(x) class(get(x, envir=.GlobalEnv))[length(class(get(x, envir=.GlobalEnv)))]))
  if(table) sapply(list_obj, function(x) length(x)) else list_obj
}


#' Applies a function over objects of a specific class
#'
#' @description Applies a function over all objects of a specific class in the global environment
#' @param object_class Class of the objects where the function is to be applied
#' @param action Name of the function to apply
#' @return Results of the function
#' @export
#' @examples
#' df1 <- data.frame(x=rnorm(10), y=rnorm(10, 1, 2))
#' df2 <- data.frame(x=rnorm(20), y=rnorm(20, 1, 2))
#' workspace_sapply("data.frame", "summary")  #Gives a summary of each data.frame
workspace_sapply <- function(object_class, action="summary"){
  sapply(workspace()[[object_class]], function(x) get(action)(get(x)), simplify=FALSE)
}

#' Check for bivariate outliers
#'
#' @description Checks for bivariate outliers in a data.frame
#' @param x A data.frame object
#' @param threshold_d Threshold for the case of two continuous variables
#' @param threshold_b Threshold for the case of one continuous and one categorical variable
#' @return A data frame with all the observations considered as bivariate outliers
#' @importFrom stats cooks.distance
#' @importFrom utils combn
#' @export
#' @examples
#' bivariate_outliers(iris)
bivariate_outliers <- function(x, threshold_d=10, threshold_b=1.5){
  pairwise_comb <- combn(1:ncol(x), 2)
  outliers <- apply(pairwise_comb, 2, function(y){
    if(all(sapply(x[,y], is.numeric))){
      mod_a <- stats::cooks.distance(lm(x[ , y[1]] ~ x[ , y[2]]))
      mod_b <- stats::cooks.distance(lm(x[ , y[2]] ~ x[ , y[1]]))
      cookD <- (mod_a+mod_b)/mean(mod_a+mod_b)
      if(any(cookD > threshold_d)){
        data.frame(row=rownames(x)[which(cookD > threshold_d)], variable1=names(x)[y[1]], value1=x[,y[1]][which(cookD > threshold_d)],
                   variable2=names(x)[y[2]], value2=x[,y[2]][which(cookD > threshold_d)])
      }
    } else{
      if(sum(sapply(x[,y], is.numeric) * rev(sapply(x[,y], is.factor))) == 1){
        factor <- sapply(x[,y], is.factor)
        case <- unsplit(lapply(split(x[,y][,!factor], x[,y][,factor]), function(x) outliers(x, threshold_b)), x[,y][,factor])
        if(any(case)){
          data.frame(row=rownames(x)[case], variable1=names(x)[y[1]], value1=as.character(x[,y[1]][case]),
                     variable2=names(x)[y[2]], value2=as.character(x[,y[2]][case]))
        }
      }
    }
  })
  output <- do.call(rbind, outliers)
  rownames(output) <- NULL
  output
}
