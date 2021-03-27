#' Computes kurtosis
#'
#' @description Calculates kurtosis of a numeric variable
#' @param x A numeric variable
#' @return kurtosis value
#' @importFrom stats sd
kurtosis <- function(x) {
  m4 <- mean((x-mean(x, na.rm=T))^4, na.rm=T)
  kurt <- m4/(sd(x, na.rm=T)^4)-3
  kurt
}

#' Computes skewness
#'
#' @description Calculates skewness of a numeric variable
#' @param x A numeric variable
#' @return skewness value
#' @importFrom stats sd
skewness <-  function(x) {
  m3 <- mean((x-mean(x, na.rm=T))^3, na.rm=T)
  skew <- m3/(sd(x, na.rm=T)^3)
  skew
}

#' Estimates number of modes
#'
#' @description Estimates the number of modes
#' @param x A numeric variable
#' @return Estimated number of modes.
#' @importFrom stats density
moda_cont <- function(x){
  if(length(na.omit(x))>1){
    maxima <- density(x, na.rm=TRUE)$y[diff(diff(density(x, adjust=1, na.rm=T)$y)>=0)<0]
    modas <- length(maxima[maxima >= max(maxima)*1/3])
    return(paste(modas, " ", sep=""))
  }
  else return(NA)
}

#' Scales data between 0 and 1
#'
#' @description Escale data to 0-1
#' @param x A numeric variable
#' @return Scaled data
scale_01 <- function(x) (x-min(x))/(max(x)-min(x))

#' Internal function for descriptive()
#'
#' @description Finds positions for substitution of characters in Distribution column
#' @param x A numeric value between 0-1
#' @param to Range of reference values
#' @return The nearest position to the input value
nearest <- function(x, to=seq(0, 1, length.out = 30)) {
  which.min(abs(to - x))
}

#' Get mode
#'
#' @description Returns the most repeated value
#' @param x A categorical variable
#' @return The mode
moda <- function(x){names(sort(-table(x)))[1]}

#' Get anti-mode
#'
#' @description Returns the least repeated value
#' @param x A categorical variable
#' @return The anti-mode (least repeated value)
antimoda <- function(x){names(sort(table(x)))[1]}

#' Gets proportion of most repeated value
#'
#' @description Returns the proportion for the most repeated value
#' @param x A categorical variable
#' @param ignore.na Should NA values be ignored for computing proportions?
#' @return A proportion
prop_may <- function(x, ignore.na=TRUE) {sort(-table(x))[1]/-(length(x)-ignore.na*sum(is.na(x)))}

#' Gets proportion of least repeated value
#'
#' @description Returns the proportion for the least repeated value
#' @param x A categorical variable
#' @param ignore.na Should NA values be ignored for computing proportions?
#' @return A proportion
prop_min <- function(x, ignore.na=TRUE){sort(table(x))[1]/(length(x)-ignore.na*sum(is.na(x)))}


#' Computes Goodman and Kruskal's tau
#'
#' @description Returns Goodman and Kruskal's tau measure of association between two categorical variables
#' @param x A categorical variable
#' @param y A categorical variable
#' @return Goodman and Kruskal's tau
#' @export
#' @examples
#' data(infert)
#' GK_assoc(infert$education, infert$case)
#' GK_assoc(infert$case, infert$education) #Not the same
GK_assoc <- function(x, y){
  Nij <- table(x, y)
  vx <- 1 - sum(rowSums(Nij/sum(Nij))^2)
  vy <- 1 - sum(colSums(Nij/sum(Nij))^2)
  d <- 1 - sum(rowSums((Nij/sum(Nij))^2)/rowSums(Nij/sum(Nij)))
  tau <- (vy - d)/vy
  return(tau)
}

#' Detailed summary of the data
#'
#' @description Creates a detailed summary of the data
#' @param x A data.frame
#' @param z Number of decimal places
#' @param ignore.na If TRUE NA values will not count for relative frequencies calculations
#' @param by Factor variable definining groups for the summary
#' @param print Should results be printed?
#' @return Summary of the data
#' @importFrom stats density dist family median na.omit quantile sd var
#' @export
#' @examples
#' descriptive(iris)
#' descriptive(iris, by="Species")
descriptive <- function(x, z=3, ignore.na=TRUE, by=NULL, print=TRUE){
  #Data.frame
  if(!is.data.frame(x)){
    x<-data.frame(x)
  }
  x<-x[, !sapply(x, function(x) all(is.na(x))), drop=FALSE]

  if(!is.null(by) && by %in% names(x)){
    by_v <- x[, by]
    x_sin <- x[, !colnames(x) == by, drop=FALSE]
    if (length(x_sin) == 0){
      descriptive(x, z, ignore.na, by=NULL)
      stop("Only one variable in the data. Can't be used as grouping variable")
    }
    x_sin <- x_sin[!is.na(by_v), ]
    by_v <- by_v[!is.na(by_v)]
    by_v <- factor(by_v)
    niveles <- levels(by_v)
    cat("Summary by ", by, ":", sep="")
    cat("\n")
    cat("-------------------------------")
    cat("\n")
    for (i in 1:length(niveles)){
      x_g <- x_sin[by_v == niveles[i], ]
      cat("Level ", by, ": ", niveles[i], sep="")
      cat("\n")
      descriptive(x=x_g, z=z, ignore.na=ignore.na, by=NULL)
      cat("\n")
      cat("-------------------------------")
      cat("\n")
    }
  } else{
    #Splitter (Splits data.frame: Numeric and categorical part)
    nums <- sapply(x, class)

    #Numeric summary
    resumen <- function(y){
      resumen1 <- round(c(min(y, na.rm=TRUE),
                          quantile(y, probs=0.25, na.rm=TRUE),
                          median(y, na.rm=TRUE),
                          quantile(y, probs=0.75, na.rm=T),
                          max(y, na.rm=TRUE),
                          mean(y, na.rm=TRUE),
                          sd(y, na.rm=TRUE),
                          kurtosis(y),
                          skewness(y)),z)
      names(resumen1) <- c("Min", "1st Q.", "Median", "3rd Q.", "Max", "Mean", "SD", "Kurtosis", "Skewness")
      distribution <- c("|", rep("-", 28), "|")
      scaled_Y <- scale_01(y)
      tryCatch(distribution[(nearest((resumen1["1st Q."] - resumen1["Min"])/
                                       (resumen1["Max"] - resumen1["Min"]))+1):
                              (nearest((resumen1["3rd Q."] - resumen1["Min"])/
                                         (resumen1["Max"] - resumen1["Min"]))-1)] <- "#", error = function(e) NA)
      tryCatch(distribution[nearest((resumen1["1st Q."] - resumen1["Min"])/
                                      (resumen1["Max"] - resumen1["Min"]))] <- "[", error = function(e) NA)
      tryCatch(distribution[nearest((resumen1["3rd Q."] - resumen1["Min"])/
                                      (resumen1["Max"] - resumen1["Min"]))] <- "]", error = function(e) NA)
      tryCatch(distribution[nearest((resumen1["Median"] - resumen1["Min"])/
                                      (resumen1["Max"] - resumen1["Min"]))] <- ":", error = function(e) NA)
      return(data.frame(t(resumen1), Modes=moda_cont(y), NAs=sum(is.na(y)),
                        Distribution = paste(distribution, collapse=""), check.names = FALSE, stringsAsFactors = FALSE))
    }
    #Character and factor summary
    resumen2 <- function(w){
      resumen2 <- c(length(table(w)),
                    abbreviate(paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/"),
                               minlength = min(20, nchar(paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/"))),
                               named=FALSE),
                    moda(w), round(prop_may(w, ignore.na = ignore.na),z), antimoda(w),
                    round(prop_min(w, ignore.na = ignore.na),z), sum(is.na(w)))
      names(resumen2)<- c("N. Classes", "Classes", "Mode", "Prop. mode", "Anti-mode", "Prop. Anti-mode", "NAs")
      data.frame(t(resumen2), check.names = FALSE, stringsAsFactors = FALSE)
    }
    #Date summary
    resumen3<-function(y){
      resumen3 <- c(as.character(min(y, na.rm=TRUE)),
                    as.character(as.Date(quantile(as.numeric(y), probs=0.25, na.rm=TRUE),
                                         origin=as.Date("1970-01-01"))),
                    as.character(median(y, na.rm=TRUE)),
                    as.character(as.Date(quantile(as.numeric(y), probs=0.75, na.rm=TRUE),
                                         origin=as.Date("1970-01-01"))),
                    as.character(max(y, na.rm=TRUE)),
                    as.character(mean(y, na.rm=TRUE)),
                    round(sd(y, na.rm=TRUE), z),
                    round(kurtosis(as.numeric(y)), z),
                    round(skewness(as.numeric(y)), z))
      names(resumen3) <- c("Min", "1st Q.", "Median", "3rd Q.", "Max", "Mean", "SD", "Kurtosis", "Skewness")
      distribution <- c("|", rep("-", 28), "|")
      scaled_Y <- scale_01(as.numeric(y))
      tryCatch(distribution[(nearest((as.numeric(as.Date(resumen3["1st Q."])) -
                                        as.numeric(as.Date(resumen3["Min"]))) /
                                      (as.numeric(as.Date(resumen3["Max"])) -
                                        as.numeric(as.Date(resumen3["Min"])))) + 1) :
                            (nearest((as.numeric(as.Date(resumen3["3rd Q."])) -
                                        as.numeric(as.Date(resumen3["Min"]))) /
                                      (as.numeric(as.Date(resumen3["Max"])) -
                                        as.numeric(as.Date(resumen3["Min"])))) - 1)] <- "#", error = function(e) NA)
      tryCatch(distribution[nearest((as.numeric(as.Date(resumen3["1st Q."])) -
                                       as.numeric(as.Date(resumen3["Min"]))) /
                                     (as.numeric(as.Date(resumen3["Max"])) -
                                       as.numeric(as.Date(resumen3["Min"]))))] <- "[", error = function(e) NA)
      tryCatch(distribution[nearest((as.numeric(as.Date(resumen3["3rd Q."])) -
                                       as.numeric(as.Date(resumen3["Min"]))) /
                                     (as.numeric(as.Date(resumen3["Max"])) -
                                       as.numeric(as.Date(resumen3["Min"]))))] <- "]", error = function(e) NA)
      tryCatch(distribution[nearest((as.numeric(as.Date(resumen3["Median"])) -
                                       as.numeric(as.Date(resumen3["Min"]))) /
                                     (as.numeric(as.Date(resumen3["Max"])) -
                                       as.numeric(as.Date(resumen3["Min"]))))] <- ":", error = function(e) NA)
      return(data.frame(t(resumen3), NAs=sum(is.na(y)), Distribution=paste(distribution, collapse=""),
                        check.names = FALSE, stringsAsFactors = FALSE))
    }

    #Results
    if(print){
    cat(paste("Data frame with", dim(x)[1], "observations and", dim(x)[2], "variables."))
    cat("\n")
    cat("\n")
    }
    if(any(nums %in% c("numeric", "integer"))){
      summary1 <- do.call(rbind, lapply(x[,nums %in% c("numeric", "integer"), drop=FALSE], resumen))
      if(print){
        cat("Numeric variables (", sum(nums %in% c("numeric", "integer")), ")", sep="")
        cat("\n")
        print(summary1)
      }
    } else { summary1 <- NULL}

    if(any(nums %in% c("factor", "character", "logical"))){
      summary2 <- do.call(rbind, lapply(x[, nums %in% c("factor", "character", "logical"), drop=FALSE], resumen2))
      if(print){
        cat("\n")
        cat("Categorical variables (", sum(nums %in% c("factor", "character", "logical")), ")", sep="")
        cat("\n")
        print(summary2, quote=FALSE)
      }
    } else { summary2 <- NULL}

    if(any(nums %in% c("Date"))){
      summary3 <- do.call(rbind, lapply(x[, nums %in% c("Date"), drop=FALSE], resumen3))
      if(print){
        cat("\n")
        cat("Date variables (", sum(nums %in% c("Date")), ")", sep="")
        cat("\n")
        print(summary3, quote=FALSE)
      }
    } else { summary3 <- NULL}

    invisible(list(numeric = summary1, character = summary2, date = summary3))
  }
}

#' Clustering of variables
#'
#' @description Displays associations between variables in a data.frame in a heatmap with clustering
#' @param x A data.frame
#' @param margins Margins for the plot
#' @return A heatmap with the variable associations
#' @importFrom stats lm heatmap xtabs
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' cluster_var(iris)
#' cluster_var(mtcars)
cluster_var <- function(x, margins=c(8,1)){
  data <- x
  if(any(sapply(data, is.numeric))){
    associations <- sapply(data[, sapply(data, is.numeric)], function(x) sapply(data, function(y) suppressWarnings(summary(lm(x ~ y))$r.squared)))
    heatmap(associations, col=colorRampPalette(c("gray", "darkred"))(25), scale="none", margins=margins, breaks=seq(0, 1, length.out = 26))
  }
  if (sum(!sapply(data, is.numeric))>1){
    associations2 <- sapply(data[, !sapply(data, is.numeric)], function(x) sapply(data[, !sapply(data, is.numeric)], function(y) GK_assoc(x, y)))
    rownames(associations2)<-colnames(associations2)
    heatmap(associations2, col=colorRampPalette(c("gray", "darkred"))(25), scale="none", margins=margins, breaks=seq(0, 1, length.out = 26))
  }
}

#' Mine plot
#'
#' @description Creates a heatmap-like plot for exploring the data
#' @param x A data.frame
#' @param fun A function that evaluates a vector and returns a logical vector
#' @param spacing Numerical separation between lines at the y-axis
#' @param sort If TRUE, variables are sorted according to their results
#' @param show.x Should the x-axis be plotted?
#' @param show.y Should the y-axis be plotted?
#' @param ... further arguments passed to order()
#' @importFrom graphics par image mtext axis
#' @importFrom methods hasArg
#' @export
#' @examples
#' mine.plot(airquality)   #Displays missing data
#' mine.plot(airquality, fun=outliers)   #Shows extreme values
mine.plot <- function(x, fun=is.na, spacing=5, sort=F, show.x=TRUE, show.y=TRUE, ...){
  x<-as.data.frame(x)
  if(sort){
    orden <- order(sapply(x, function(x) sum(fun(x))), ...)
    x <- x[,orden]
  }
  old.warn <- options(warn = -1)
  pad <- ceiling(dim(x)[2]/30)
  old.par <- par(mar=c(8, 4.5, 6, 4))
  image(t(sapply(x, fun)), xaxt="n", yaxt="n", col=colorRampPalette(c("lightcyan4", "darkred"))(2), ...)
  if(show.x){
    axis(1, at=seq(0, 1, length=dim(x)[2]), labels=paste(names(x), "\n", "(", sapply(x, class), ")", sep=""), las=2, lwd=0, cex.axis=0.8)
  }
  if(show.y){
    axis(2, at = seq(0, dim(x)[1], by = spacing)/dim(x)[1], labels = seq(0, dim(x)[1], by = spacing), las = 1, cex.axis = 0.6)
  }
  for(i in 1:pad){
    axis(3, at=seq(0, 1, length=dim(x)[2])[seq(0 + i, dim(x)[2], by=pad)],
         labels=sapply(x, function(x) round(100*sum(fun(x))/length(x)))[seq(0+i, dim(x)[2], by=pad)], cex.axis=0.6, lwd=0, line=-1+i/2)
  }
  if(!hasArg("main")) mtext(paste("%", as.character(substitute(fun))), 3, line=max(pad/1.5, 2.5), cex=1.2)
  options(old.warn)
  output1 <- do.call(rbind, lapply(1:ncol(x), function(y){
    variable <- names(x)[y]
    out <- fun(x[,y])
    id <- which(out)
    value <- x[,y][out]
    if(ttrue(any(out))) data.frame(variable = variable, id = id, value = as.character(value))
  }))
  output2 <- sapply(x, function(x) round(sum(fun(x))/length(x), 2))
  return(list(list = output1, summary = output2))
  par(old.par)
}

#' outliers
#'
#' @description Function for detecting outliers based on the boxplot method
#' @param x A vector
#' @export
#' @examples
#' outliers(iris$Petal.Length)
#' outliers(airquality$Ozone)
outliers <- function(x){
  if(any(c("Date", "POSIXt") %in% class(x))){
    quantiles <- quantile(as.POSIXct(x), probs=c(0.25, 0.75), na.rm=TRUE)
    unclass(as.POSIXct(x)) %<NA% (unclass(quantiles["25%"]) - 1.5*(unclass(quantiles["75%"])-unclass(quantiles["25%"]))) |
      unclass(as.POSIXct(x)) %>NA% (unclass(quantiles["75%"]) + 1.5*(unclass(quantiles["75%"])-unclass(quantiles["25%"])))
  } else{
    if(is.numeric(x)){
      quantiles <- quantile(x, probs=c(0.25, 0.75), na.rm=TRUE)
      x %<NA% (quantiles["25%"] - 1.5*(quantiles["75%"]-quantiles["25%"])) |
        x %>NA% (quantiles["75%"] + 1.5*(quantiles["75%"]-quantiles["25%"]))
    } else rep(NA, length(x))
  }
}


#' Improved boxplot
#'
#' @description Creates an improved boxplot with individual data points
#' @param formula Formula for the boxplot
#' @param boxwex Width of the boxes
#' @param ... further arguments passed to beeswarm()
#' @importFrom grDevices rgb
#' @export
#' @examples
#' ipboxplot(Sepal.Length ~ Species, data=iris)
#' ipboxplot(mpg ~ gear, data=mtcars)
ipboxplot<-function(formula, boxwex=0.6, ...){
  boxplot(formula, las=1, cex.axis=1.2, cex.lab=1.2, boxwex=boxwex, ...)
  beeswarm::beeswarm(formula, pch=16, col=rgb(50, 50, 50, 150, maxColorValue=255), add=T, ...)
}

#' Multiple tapply
#'
#' @description Modification of the tapply function to use with data.frames. Consider using aggregate()
#' @param x A data.frame
#' @param group Grouping variable
#' @param fun Function to apply by group
#' @export
#' @examples
#' mtapply(mtcars, mtcars$gear, mean)
mtapply <- function(x, group, fun){
  if(is.null(dim(x))) tapply(x, group, fun)
  else sapply(split(x, group), function(x) sapply(x, function(x) fun(x)))
}

#' Peek
#'
#' @description Takes a peek into a data.frame returning a concise visualization about it
#' @param x A data.frame
#' @param n Number of rows to include in output
#' @param which Columns to include in output
#' @importFrom utils head
#' @export
#' @examples
#' peek(iris)
peek <- function(x, n=10, which=1:ncol(x)){
  class <- sapply(x[,which], class)
  range <- paste("(", sapply(x[,which], function(x) {
    ifelse(class(x) %in% c("character", "factor"), length(unique(x)),
           ifelse(class(x) %in% c("Date") | is.numeric(x), paste(round(range(x, na.rm=TRUE),2), collapse="; "),
                  ""))
  }
  ), ")", sep="")
  blank <- rep("", length=length(class))
  output <- rbind(as.matrix(head(x[,which], n)), rep("", ncol(x[,which])), class, range, blank, blank)
  rownames(output)[(n+4):(n+5)]<-""
  cat("Data frame with ", nrow(x), " rows (showing ", length(which), " of ", ncol(x), " variables) \n \n")
  print(output, quote = FALSE)
}

#' Kill factors
#'
#' @description Changes factor variables to character
#' @param dat A data.frame
#' @param k Maximum number of levels for factors
#' @export
#' @examples
#' d <- data.frame(Letters=letters[1:20], Nums=1:20)
#' d$Letters
#' d <- kill.factors(d)
#' d$Letters
kill.factors <- function(dat, k=10){
  filter <- sapply(dat, function(x) is.factor(x) & length(levels(x))>k)
  dat[filter] <- lapply(dat[filter], as.character)
  return(dat)
}

#' Good to go
#'
#' @description Loads all libraries used in scripts inside the selected path
#' @param path Path where the scripts are located
#' @param info List the libraries found?
#' @param load Should the libraries found be loaded?
#' @export
good2go <- function(path=getwd(), info=TRUE, load=TRUE){
  files <- list.files(path=path, pattern="\\.R$")
  libraries <- unique(do.call("c", lapply(files, function(x) {
    x <- readLines(x)
    x[grepl("library\\(|require\\(", x)]
  }
  )))
  p_list <- gsub('\\"', "", gsub("\\)", "", gsub("library\\(|require\\(", "", libraries)))
  if(load) lapply(p_list, function(x) require(x, character.only = TRUE, quietly=TRUE))
  if(info) print(paste("Packages:", paste(p_list, collapse=", ")), quote=FALSE)
}

#' Forge
#'
#' @description Reshapes a data frame from wide to long format
#' @param data data.frame
#' @param affixes Affixes for repeated measures
#' @param force.fixed Variables with matching affix to be excluded
#' @param var.name Name for the new created variable (repetitions)
#' @export
#' @examples
#' #Data frame in wide format
#' df1 <- data.frame(id = 1:4, age = c(20, 30, 30, 35), score1 = c(2,2,3,4),
#'                   score2 = c(2,1,3,1), score3 = c(1,1,0,1))
#' df1
#' #Data frame in long format
#' forge(df1, affixes= c("1", "2", "3"))
#'
#' #Data frame in wide format with two repeated measured variables
#' df2 <- data.frame(df1, var1 = c(15, 20, 16, 19), var3 = c(12, 15, 15, 17))
#' df2
#' #Missing times are filled with NAs
#' forge(df2, affixes = c("1", "2", "3"))
#'
#' #Use of parameter force.fixed
#' df3 <- df2[, -7]
#' df3
#' forge(df3, affixes=c("1", "2", "3"))
#' forge(df3, affixes=c("1", "2", "3"), force.fixed = c("var1"))
forge <- function(data, affixes, force.fixed = NULL, var.name = "time"){
  data_ord <- data[, order(names(data))]
  indices <- data.frame(sapply(affixes, function(x) grepl(x, names(data_ord))))
  if(!is.null(force.fixed)){
    positions <- which(names(data_ord) %in% force.fixed)
    indices[positions, ] <- FALSE
  }
  cat("Repetitions for each variable: \n \n")
  print(sort(table(unlist(lapply(indices, function(x) gsub(paste(affixes,
    collapse = "|"), "", names(data_ord)[x])))), decreasing = TRUE))
  listas <- lapply(indices, function(x) {
    setNames(data_ord[, x, drop = FALSE], gsub(paste(affixes,
      collapse = "|"), "", names(data_ord)[x]))
  })
  variables <- unique(unlist(lapply(listas, names)))
  listas <- lapply(listas, function(x) {
    df <- data.frame(x, matrix(NA, nrow = nrow(data), ncol = length(variables[!variables %in%
                                                                                names(x)])))
    names(df) <- c(names(x), variables[!variables %in% names(x)])
    df
  })
  long <- do.call("rbind", listas)
  fixed <- data_ord[, !apply(indices, 1, any), drop=FALSE]
  out <- data.frame(fixed[, na.omit(match(names(data), names(fixed))), drop=FALSE][rep(1:nrow(data),
    length(affixes)), ,drop=FALSE], long, affix = rep(affixes, each = nrow(data)))
  names(out)[ncol(out)] <- var.name
  out
}

#' Un-Forge
#'
#' @description Reshapes a data frame from long to wide format
#' @param data data.frame
#' @param origin Character vector with variable names in data containing the values to be assigned to the different new variables
#' @param variables Variable in data containing the variable names to be created
#' @param prefix Vector with prefixes for the new variable names
#' @export
#' @examples
#' #Data frame in wide format
#' df1 <- data.frame(id = 1:4, age = c(20, 30, 30, 35), score1 = c(2,2,3,4),
#'                   score2 = c(2,1,3,1), score3 = c(1,1,0,1))
#' df1
#' #Data frame in long format
#' df2 <- forge(df1, affixes= c("1", "2", "3"))
#' df2
#' #Data frame in wide format again
#' df3 <- unforge(df2, "score", "time", prefix="score")
#'
unforge <- function(data, origin, variables, prefix=origin){
  splitted <- split(data, data[variables])
  out_data <- cbind(splitted[[1]][,!names(data) %in% c(origin, variables), drop=FALSE],
                    setNames(do.call(cbind, lapply(split(data, data[variables]), function(x) x[origin])),
                             paste(rep(prefix, length(unique(data[,variables]))),
                                   rep(names(splitted), each=length(origin)), sep="_")))
  out_data
}

#' Search scripts
#'
#' @description Searches for strings in R script files
#' @param string Character string to search
#' @param path Character vector with the path name
#' @param recursive Logical. Should the search be recursive into subdirectories?
#' @return A list with each element being one of the files containing the search string
#' @export
search_scripts <- function(string, path=getwd(), recursive=TRUE){
  files <- list.files(path=path, pattern="\\.R$", recursive = recursive, full.names=TRUE)
  listado <- lapply(files, function(x) {
    x <- readLines(x, warn=FALSE)
    x[grepl(string, x)]
  })
  names(listado) <- files
  listado[sapply(listado, length) != 0]
}
