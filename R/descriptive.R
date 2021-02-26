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
#' @description Returns Goodman and Kruskal's tay measure of association between two categorical variables
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
#' @return Summary of the data
#' @importFrom stats density dist family median na.omit quantile sd var
#' @export
#' @examples
#' descriptive(iris)
#' descriptive(iris, by="Species")
descriptive <- function(x, z=3, ignore.na=TRUE, by=NULL){
  #Data.frame
  if(!is.data.frame(x)){
    x<-data.frame(x)
  }
  x<-x[, !sapply(x, function(x) all(is.na(x)))]

  if(!is.null(by) && by %in% names(x)){
    by_v <- x[,by]
    x_sin <- x[,!colnames(x) == by, drop=FALSE]
    if (length(x_sin)==0){
      descriptive(x,z,ignore.na,by=NULL)
      stop("Only one variable in the data. Can't be used as grouping variable")
    }
    x_sin <- x_sin[!is.na(by_v),]
    by_v <- by_v[!is.na(by_v)]
    by_v <- factor(by_v)
    niveles <- levels(by_v)
    cat("Summary by ", by, ":", sep="")
    cat("\n")
    cat("-------------------------------")
    cat("\n")
    for (i in 1:length(niveles)){
      x_g <- x_sin[by_v==niveles[i],]
      cat("Level ", by, ": ", niveles[i], sep="")
      cat("\n")
      descriptive(x=x_g,z=z,ignore.na=ignore.na,by=NULL)
      cat("\n")
      cat("-------------------------------")
      cat("\n")
    }
  } else{
    #Splitter (Splits data.frame: Numeric and categorical part)
    nums <- sapply(x, is.numeric)

    #Numeric summary
    resumen<-function(y){
      resumen1 <- round(c(min(y, na.rm=T), quantile(y, probs=0.25, na.rm=T), median(y, na.rm=T), quantile(y, probs=0.75, na.rm=T), max(y, na.rm=T), mean(y, na.rm=T), sd(y, na.rm=T), kurtosis(y), skewness(y)),z)
      names(resumen1) <- c("Min", "1st Q.", "Median", "3rd Q.", "Max", "Mean", "SD", "Kurtosis", "Skewness")
      distribution <- c("|", rep("-", 28), "|")
      scaled_Y <- scale_01(y)
      tryCatch(distribution[(nearest((resumen1["1st Q."]-resumen1["Min"])/(resumen1["Max"]-resumen1["Min"]))+1):(nearest((resumen1["3rd Q."]-resumen1["Min"])/(resumen1["Max"]-resumen1["Min"]))-1)]<-"#", error=function(e) NA)
      tryCatch(distribution[nearest((resumen1["1st Q."]-resumen1["Min"])/(resumen1["Max"]-resumen1["Min"]))]<-"[", error=function(e) NA)
      tryCatch(distribution[nearest((resumen1["3rd Q."]-resumen1["Min"])/(resumen1["Max"]-resumen1["Min"]))]<-"]", error=function(e) NA)
      tryCatch(distribution[nearest((resumen1["Median"]-resumen1["Min"])/(resumen1["Max"]-resumen1["Min"]))]<-":", error=function(e) NA)
      return(data.frame(t(resumen1), Modes=moda_cont(y), NAs=sum(is.na(y)), Distribution=paste(distribution, collapse=""), check.names = FALSE, stringsAsFactors = FALSE))
    }

    resumen2<-function(w){
      resumen2<-c(length(table(w)), abbreviate(paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/"), minlength = min(20, nchar(paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/"))), named=FALSE), moda(w), round(prop_may(w, ignore.na = ignore.na),z), antimoda(w), round(prop_min(w, ignore.na = ignore.na),z), sum(is.na(w)))
      names(resumen2)<- c("N. Classes", "Classes", "Mode", "Prop. mode", "Anti-mode", "Prop. Anti-mode", "NAs")
      data.frame(t(resumen2), check.names = FALSE, stringsAsFactors = FALSE)
    }
    #Results
    cat(paste("Data frame with", dim(x)[1], "observations and", dim(x)[2], "variables."))
    cat("\n")
    cat("\n")
    if("TRUE" %in% nums){
      cat("Numeric variables (", sum(nums), ")", sep="")
      cat("\n")
      summary1 <- do.call(rbind, lapply(x[,nums], resumen))
      print(summary1)
    }
    if("FALSE" %in% nums){
      summary2 <- do.call(rbind, lapply(x[,!nums, drop=FALSE], resumen2))
      cat("\n")
      cat("Categorical variables (", dim(x)[2]-sum(nums), ")", sep="")
      cat("\n")
      print(summary2, quote=FALSE)
    } else { summary2 <- NULL}
    invisible(list(numeric=summary1, character=summary2))
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
#' @importFrom graphics par image mtext
#' @importFrom methods hasArg
#' @export
#' @examples
#' mine.plot(airquality)   #Displays missing data
#' mine.plot(airquality, fun=outliers)   #Shows extreme values
mine.plot <- function(x, fun=is.na, spacing=5, sort=F, show.x=TRUE, show.y=TRUE, ...){
  x<-as.data.frame(x)
  if(sort){
    orden <- order(sapply(x, function(x) sum(is.it(x))), ...)
    x <- x[,orden]
  }
  old.warn <- options(warn=-1)
  pad<- ceiling(dim(x)[2]/30)
  old.par <- par(mar=c(8, 4.5, 6, 4))
  image(t(sapply(x, fun)), xaxt="n", yaxt="n", col=colorRampPalette(c("lightcyan4", "darkred"))(2), ...)
  if(show.x){
    axis(1, at=seq(0, 1, length=dim(x)[2]), labels=paste(names(x), "\n", "(", sapply(x, class), ")", sep=""), las=2, lwd=0, cex.axis=0.8)
  }
  if(show.y){
    axis(2, at=seq(0, dim(x)[1], by=spacing)/dim(x)[1], labels=seq(0, dim(x)[1], by=spacing), las=1, cex.axis=0.6)
  }
  for(i in 1:pad){
    axis(3, at=seq(0, 1, length=dim(x)[2])[seq(0+i, dim(x)[2], by=pad)],
         labels=sapply(x, function(x) round(100*sum(fun(x))/length(x)))[seq(0+i, dim(x)[2], by=pad)], cex.axis=0.6, lwd=0, line=-1+i/2)
  }
  if(!hasArg("main")) mtext(paste("%", as.character(substitute(fun))), 3, line=max(pad/1.5, 2.5), cex=1.2)
  options(old.warn)
  output1 <- do.call(rbind, lapply(1:ncol(x), function(y){
    variable <- names(x)[y]
    out <- fun(x[,y])
    id <- which(out)
    value <- x[,y][out]
    if(ttrue(any(out))) data.frame(variable=variable, id=id, value=as.character(value))
  }))
  output2 <- sapply(x, function(x) round(sum(fun(x))/length(x), 2))
  return(list(list=output1, summary=output2))
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

#' Auxiliary matrix paste function
#' @description Internal function for report.table
#' @param ... Matrices to paste
#' @param sep Separator for the paste function
matrixPaste<-function (..., sep = rep(" ", length(list(...)) - 1)){
  theDots <- list(...)
  if (any(unlist(lapply(theDots, function(x) !is.character(x)))))
    stop("all matrices must be character")
  numRows <- unlist(lapply(theDots, nrow))
  numCols <- unlist(lapply(theDots, ncol))
  if (length(unique(numRows)) > 1 | length(unique(numCols)) >
      1)
    stop("all matrices must have the same dim")
  for (i in seq(along = theDots)) out <- if (i == 1)
    theDots[[i]]
  else paste(out, theDots[[i]], sep = sep[i - 1])
  matrix(out, nrow = numRows[1])
}

#' Report tables of summary data
#'
#' @description Creates a report table ready for publication
#' @param x A data.frame object
#' @param by Grouping variable for the report
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimal places
#' @param digitscat Number of decimal places for categorical variables (if different to digits)
#' @param print Should the report table be printed on screen?
#' @param ... further arguments passed to make_table()
#' @export
#' @examples
#' report(iris)
#' (reporTable<-report(iris, by="Species"))
#' class(reporTable)
report.data.frame<-function(x, by=NULL, file=NULL, type="word", digits=2, digitscat=digits, print=TRUE, ...){
  if(is.data.frame(x)==F){
    x<-data.frame(x)}
  x<-x[,!sapply(x, function(x) sum(is.na(x))/length(x))==1 & sapply(x, function(x) is.numeric(x) | is.factor(x)), drop=FALSE]
  x[sapply(x, is.factor) & sapply(x, function(x) !all(levels(x) %in% unique(na.omit(x))))]<-lapply(x[sapply(x, is.factor) & sapply(x, function(x) !all(levels(x) %in% unique(na.omit(x))))], factor)
  if(length(by)>1){
    x.int <- data.frame(x, by=interaction(x[, match(unlist(by), names(x))]))
    report(x.int[,-match(unlist(by), names(x.int))], by="by", file=file, type=type, digits=digits, digitscat=digitscat, ...)
  }
  else{
  by_v <- factor(rep("", nrow(x)))
  if(!is.null(by)){
    pos_by<-match(by, names(x))
    by_v<-factor(eval(parse(text=paste("x$", by, sep=""))))
    x<-x[,-pos_by, drop=FALSE]
  }

  #Numeric part
  nums <- sapply(x, is.numeric)
  if(any(nums==TRUE)){
    estruct<-matrix(nrow=2, ncol=length(unique(na.omit(by_v)))+1)
    estruct[1:2,1]<-c("", "")
    estruct[1, -1]<-paste("Mean (SD)", ifelse(any(nums==FALSE), " / n(%)", ""), sep="")
    estruct[2,-1]<-"Median (1st, 3rd Q.)"
    cont<-character(2*length(x[nums==T]))
    cont[seq(1,length(cont), 2)]<-colnames(x[,nums==T, drop=FALSE])
    if(ncol(x[,nums==T, drop=FALSE])>1){
      A<-matrixPaste(sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(mean(x, na.rm=TRUE),digits)))), function(x) t(x)), " (",
                     sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(sd(x, na.rm=TRUE),digits)))), function(x) t(x)),")", sep=rep("", 3))

      B<-matrixPaste(sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(median(x, na.rm=TRUE),digits)))), function(x) t(x)),
                     " (",
                     sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(quantile(x, 0.25, na.rm=TRUE),digits)))), function(x) t(x)),
                     ", ",
                     sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(quantile(x, 0.75, na.rm=TRUE),digits)))), function(x) t(x)),
                     ")", sep=rep("", 5))
    }
    else {
      A<-paste(sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(mean(x, na.rm=TRUE),digits)))), function(x) t(x)), " (",
               sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(sd(x, na.rm=TRUE),digits)))), function(x) t(x)),")", sep=rep(""))
      B<-paste(sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(median(x, na.rm=TRUE),digits)))), function(x) t(x)),
               " (",
               sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(quantile(x, 0.25, na.rm=TRUE),digits)))), function(x) t(x)),
               ", ",
               sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(quantile(x, 0.75, na.rm=TRUE),digits)))), function(x) t(x)),
               ")", sep=rep(""))
    }

    AB<-matrix(nrow=nrow(rbind(A, B)), ncol=ncol(rbind(A,B))+1)
    AB[seq(1, dim(rbind(A, B))[1], 2),-1]<-A
    AB[-c(seq(1, dim(rbind(A, B))[1], 2)),-1]<-B
    AB[,1]<-cont
  }
  else{
    AB<-NULL
    estruct<-matrix(nrow=1, ncol=length(unique(na.omit(by_v)))+1)
    estruct[1,1]<-""
    estruct[1, -1]<-"n (%)"
  }

  #Categorical part
  cats<-matrix(data="", ncol=length(levels(by_v))+1, nrow=suppressWarnings(length(na.omit(unlist(sapply(x[nums==F], function(x) na.omit(unique(x)))))))+length(x[nums==F]))
  pos<-sapply(sapply(x[nums==F], function(x) na.omit(unique(x)), simplify=FALSE), function(x) length(x))
  cats[rev(rev(cumsum(c(1,pos)))[-1])+rev(rev((0:(dim(x[nums==F])[2])))[-1]),1]<-colnames(x[nums==F])
  cats[-(rev(rev(cumsum(c(1,pos)))[-1])+rev(rev((0:(dim(x[nums==F])[2])))[-1])),1]<-paste("  ", suppressWarnings(na.omit(unlist(sapply(x[nums==F], function(x) levels(as.factor(x)))))), sep="")
  if(any(nums==FALSE)){
    x[nums==F] <- lapply(x[nums==F],as.factor)
    C<-matrixPaste(sapply(by(x[nums==F], by_v, function(x) sapply(x, function(x) as.character(table(x)))), function(x) unlist(x)), " (",
                   sapply(by(x[nums==F], by_v, function(x) sapply(x, function(x) as.character(round(100*(table(x)/sum(table(x))),digitscat)))), function(x) unlist(x)),"%)", sep=rep("", 3))
    cats[-(rev(rev(cumsum(c(1,pos)))[-1])+rev(rev((0:(dim(x[nums==F])[2])))[-1])),-1]<-C
  }

  #Matrix binding
  output<-rbind(estruct, AB, cats)
  colnames(output)<-c("Variable", paste(by, levels(by_v), sep=" ", "n =", as.vector(table(by_v))))
  if(!is.null(file)) make_table(output, file, type, use.rownames=FALSE)
  output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(output, row.names=FALSE, right=FALSE)
  invisible(output)
  }
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

#' Fix factors imported as numerics
#'
#' @description Fixes factors imported as numerics
#' @param x A data.frame
#' @param k Maximum number of numeric values to be converted to factor
#' @param drop Drop similar levels?
#' @param track Keep track of changes?
#' @export
#' @examples
#' report(mtcars)
#' report(fix.factors(mtcars))
fix.factors<-function(x, k=5, drop=TRUE, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  candidate_variables <- (sapply(x, function(x) (is.numeric(x) |
                                                   is.character(x)) &
                                   length(unique(x))<=k)) |
    (sapply(x, function(x) is.factor(x)))
  x[, candidate_variables] <- lapply(x[, candidate_variables, drop=FALSE],
                                     function(x) {
                                       if(drop) {factor(iconv(droplevels(as.factor(gsub("^ *|(?<= ) | *$", "", tolower(as.character(x)), perl=TRUE))), to="ASCII//TRANSLIT"))
                                       } else factor(x)
                                     })
  if(!identical(old, x)){
    if(track){
    changes <- data.frame(variable=names(candidate_variables)[candidate_variables],
                          observation="all",
                          original=sapply(old[,candidate_variables, drop=FALSE], class),
                          new="factor",
                          fun="fix.factors", row.names=NULL)
    if(!is.null(changes_old)){
      attr(x, "changes") <- rbind(changes_old, changes)
    } else {
      attr(x, "changes") <- changes
    }
  }
  return(x)
  } else return(old)
}

#' Fix numeric data
#'
#' @description Fixes numeric data
#' @param x A data.frame
#' @param k Minimum number of different values to be considered numerical
#' @param max.NA Maximum allowed proportion of NA values created by coercion
#' @param track Keep track of changes?
#' @export
#' @examples
#' mydata<-data.frame(Numeric1=c(7.8, 9.2, 5.4, 3.3, "6,8", "3..3"),
#'                    Numeric2=c(3.1, 1.2, "3.s4", "a48,s5", 7, "6,,4"), stringsAsFactors=TRUE)
#' report(mydata)
#' report(fix.numerics(mydata, k=5))
fix.numerics <- function(x, k=8, max.NA=0.2, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  previous.NA<- sapply(x, function(x) sum(is.na(x)))
  candidate_variables <- apply(sapply(x, function(x) grepl("[0-9]", as.character(x))), 2, any) & sapply(x, function(x) !(is.numeric(x) | inherits(x, 'Date'))) & sapply(x, function(x) length(unique(x))>=k)
  percent_variables <- apply(sapply(x, function(x) grepl("%", as.character(x))), 2, any)
  sci_notation_variables <- apply(sapply(x, function(x) grepl("[0-9](e|E)([0-9]|-[0-9]|\\+[0-9])", as.character(x))), 2, any)
  thousand_separators <- apply(sapply(x, function(x) grepl(".\\..{3},", x) | grepl(".,.{3}\\.", x) | grepl(".\\..{3}\\.", x) | grepl(".\\,.{3}\\,", x)), 2, any)
  x[, candidate_variables & percent_variables] <- lapply(x[, candidate_variables & percent_variables, drop=FALSE], function(x){
    x[grepl("%", x)] <- numeros(gsub("%", "", x[grepl("%", x)]))/100
    x})
  x[, candidate_variables & sci_notation_variables] <- lapply(x[, candidate_variables & sci_notation_variables, drop = FALSE],
                                                              function(x){
                                                                mult_factor <- sapply(strsplit(x, "e|E"), function(x) 10^as.numeric(x[2]))
                                                                mult_factor[is.na(mult_factor)] <- 1
                                                                base <- sapply(strsplit(x, "e|E"), function(x) x[1])
                                                                numeros(base)*mult_factor
                                                              })
  x[, thousand_separators & candidate_variables] <- lapply(x[, candidate_variables & thousand_separators, drop=FALSE], function(x){
    point_thousands <- sum(grepl("\\..*,", x) | grepl("\\..{3}\\.", x))
    comma_thousands <- sum(grepl(",.*\\.", x) | grepl(",.{3},", x))
    if(point_thousands > comma_thousands){
      x[grepl("\\.", x) & (!(grepl("\\..{3}$", x) | grepl("\\..{3},", x)) | grepl(",.*\\.", x))] <- NA
      x <- gsub("\\.", "", x)
    }
    if(comma_thousands > point_thousands){
      x[grepl(",", x) & (!(grepl(",.{3}$", x) | grepl(",.{3}.", x)) | grepl("\\..*,", x))] <- NA
      x <- gsub(",", "", x)
    }
    x
  })
  x[, candidate_variables] <- lapply(x[, candidate_variables, drop=FALSE], function(x) numeros(x))
  final.NA<-sapply(x, function(x) sum(is.na(x)))-previous.NA
  x[,(final.NA-previous.NA) > nrow(x)*max.NA] <- old[,(final.NA-previous.NA) > nrow(x)*max.NA]
  print(paste(sum(sapply(x, function(x) sum(is.na(x)))-previous.NA), "new missing values generated"))
  print(paste(sum((final.NA-previous.NA) > nrow(x)*max.NA), "variables excluded following max.NA criterion"))
  if(!identical(old, x)){
    if(track){
      changes1 <- data.frame(variable=names(candidate_variables[candidate_variables & !((final.NA-previous.NA) > nrow(x)*max.NA)]),
                             observation="all",
                             original=sapply(old[,candidate_variables & !((final.NA-previous.NA) > nrow(x)*max.NA), drop=FALSE], class),
                             new="numeric",
                             fun="fix.numerics",
                             row.names=NULL)
      changes2 <- do.call(rbind, lapply(changes1$variable, function(y){
        observations <- which(!(old[, y] %in% x[, y]))
        tryCatch(data.frame(variable=y,
                            observation=observations,
                            original=old[observations, y],
                            new=x[observations, y],
                            fun="fix.numerics"), error = function(e) NULL)
      }))
      changes <- rbind(changes1, changes2)
      if(!is.null(changes_old)){
        attr(x, "changes") <- rbind(changes_old, changes)
      } else {
        attr(x, "changes") <- changes
      }
    }
    return(x)
  } else return(old)
}


#' Fix dates
#'
#' @description Fixes dates
#' @param x A data.frame
#' @param max.NA Maximum allowed proportion of NA values created by coercion
#' @param min.obs Minimum number of non-NA observations allowed per variable
#' @param locale Locale to be used for month names
#' @param use.probs Solve ambiguities by similarity to the most frequent formats
#' @param track Track changes?
#' @export
#' @examples
#' mydata<-data.frame(Dates1=c("25/06/1983", "25-08/2014", "2001/11/01", "2008-10-01"),
#'                    Dates2=c("01/01/85", "04/04/1982", "07/12-2016", NA),
#'                    Numeric1=rnorm(4))
#' fix.dates(mydata)
fix.dates <- function (x, max.NA=0.8, min.obs=nrow(x)*0.05, locale="C", use.probs=TRUE, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  x<-kill.factors(x)
  x.old<-x
  previous.NA <- sapply(x, function(x) sum(is.na(x)))
  previous.minobs <- sum(sapply(x, function(x) sum(!is.na(x))<min.obs))
  candidate_variables <- apply(sapply(x, function(x) grepl("(-{1}|/{1}).{1,4}(-{1}|/{1})", as.character(x))), 2, any)
  x[, candidate_variables] <- lapply(x[, candidate_variables, drop = FALSE], function(x) fxd(x, locale=locale, use.probs=use.probs))
  final.NA <- sapply(x, function(x) sum(is.na(x))) - previous.NA
  final.minobs<-sum(sapply(x, function(x) sum(!is.na(x))<min.obs))
  x[,((final.NA-previous.NA) > nrow(x)*max.NA) | sapply(x, function(x) sum(!is.na(x))<min.obs)]<-x.old[,((final.NA-previous.NA) > nrow(x)*max.NA) | sapply(x, function(x) sum(!is.na(x))<min.obs)]
  print(paste(sum(sapply(x, function(x) sum(is.na(x)))-previous.NA), "new missing values generated"))
  print(paste(sum((final.NA-previous.NA) > nrow(x)*max.NA), "variables excluded following max.NA criterion"))
  print(paste(final.minobs-previous.minobs, "variables excluded following min.obs criterion"))
  final_variables <- candidate_variables & !(((final.NA-previous.NA) > nrow(x)*max.NA) | sapply(x, function(x) sum(!is.na(x))<min.obs))
  if(track & sum(final_variables)>0){
    changes1 <- data.frame(variable=names(final_variables[final_variables]),
                           observation="all",
                           original=sapply(old[,final_variables, drop=FALSE], class),
                           new="Date",
                           fun="fix.dates",
                           row.names=NULL)
    changes2 <- do.call(rbind, lapply(changes1$variable, function(y){
      observations <- which(!(old[, y] %in% x[, y]))
      tryCatch(data.frame(variable=y,
                          observation=observations,
                          original=old[observations, y],
                          new=as.character(x[observations, y]),
                          fun="fix.dates"), error = function(e) NULL)
    }))
    changes <- rbind(changes1, changes2)
    if(!is.null(changes_old)){
      attr(x, "changes") <- rbind(changes_old, changes)
    } else {
      attr(x, "changes") <- changes
    }
  }
  return(x)
}

#' Internal function to fix.dates
#'
#' @description Function to format dates
#' @param d A character vector
#' @param locale Locale to be used for month names
#' @param use.probs Solve ambiguities by similarity to the most frequent formats
fxd <- function(d, locale="C", use.probs=TRUE){
  formats <- c("%d-%m-%Y", "%d-%m-%y", "%Y-%m-%d", "%m-%d-%Y", "%m-%d-%y", "%d-%b-%Y", "%d-%B-%Y", "%d-%b-%y", "%d-%B-%y",
               "%d%m%Y", "%d%m%y", "%Y%m%d", "%m%d%Y", "%m%d%y", "%d%b%Y", "%d%B%Y", "%d%b%y", "%d%B%y")
  d[grep("ene", d)]<-gsub("ene", "jan", d[grep("ene", d)])
  d[grep("abr", d)]<-gsub("abr", "apr", d[grep("abr", d)])
  d[grep("ago", d)]<-gsub("ago", "aug", d[grep("ago", d)])
  d[grep("dic", d)]<-gsub("dic", "dec", d[grep("dic", d)])
  Sys.setlocale("LC_TIME", locale)
  prueba <- lapply(formats, function(x) as.Date(tolower(gsub("--", "-", gsub('[[:punct:]]','-',d))), format=x))
  co <-lapply(prueba, function(x) {
    x[format.Date(x, "%Y")<100]<-NA
    return(x)
  })
  to.NA <- which(sapply(d, function(x) nchar(as.character(gsub("[[:alpha:]]+", "xx", x)))>8))
  co[c(2, 5, 8, 9, 11, 14, 17, 18)] <- lapply(co[c(2, 5, 8, 9, 11, 14, 17, 18)], function(x){
    x[to.NA]<-NA
    return(x)
  })
  if(use.probs){
    co<-co[order(unlist(lapply(co, function(x) sum(is.na(x)))))]
  }
  final_dates <- do.call("c", lapply(1:length(d), function(y) na.omit(do.call("c", lapply(co, function(x) x[y])))[1]))
  years <- as.numeric(substr(final_dates, 1, 4))
  median_year <- median(years, na.rm=TRUE)
  final_dates[abs(years - median_year) %>NA% abs(years-100 - median_year)] <- do.call(c, lapply(final_dates[abs(years - median_year) %>NA% abs(years-100 - median_year)], function(x) tryCatch(seq(x, length=2, by="-100 years")[2], error=function(e) NA)))
  final_dates[abs(years - median_year) %>NA% abs(years+100 - median_year)] <- do.call(c, lapply(final_dates[abs(years - median_year) %>NA% abs(years+100 - median_year)], function(x) tryCatch(seq(x, length=2, by="100 years")[2], error=function(e) NA)))
  return(final_dates)
  Sys.setlocale("LC_TIME", "")
}

#' Fix levels
#'
#' @description Fixes levels of a factor
#' @param data data.frame with the factor to fix
#' @param factor_name Name of the factor to fix (as character)
#' @param method Method from stringdist package to estimate distances
#' @param levels Optional vector with the levels names
#' @param plot Optional: Plot cluster dendrogram?
#' @param k Number of levels for clustering
#' @param track Keep track of changes?
#' @param ... Further parameters passed to stringdist::stringdistmatrix function
#' @importFrom stats hclust rect.hclust cutree
#' @export
#' @examples
#' mydata <- data.frame(factor1=factor(c("Control", "Treatment", "Tretament", "Tratment", "treatment",
#' "teatment", "contrl", "cntrol", "CONTol", "not available", "na")))
#' fix.levels(mydata, "factor1", k=4, plot=TRUE)   #Chose k to select matching levels
#' fix.levels(mydata, "factor1", levels=c("Control", "Treatment"), k=4)
fix.levels <- function(data, factor_name, method="dl", levels=NULL, plot=FALSE, k=ifelse(!is.null(levels), length(levels), 2), track=TRUE, ...){
  changes_old <- attr(data, "changes")
  x <- data[,factor_name]
  x_na <- na.omit(x)
  if(method %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")){
    clusters <- hclust(stringdist::stringdistmatrix(tolower(x_na), method=method, useNames=TRUE, ...))
    if(plot){
      clusplot <- hclust(stringdist::stringdistmatrix(unique(tolower(x_na)), method=method, useNames=TRUE, ...))
      plot(clusplot)
      rect.hclust(clusplot, k=k, border="red")
    }
  } else{
    stop("Method should be one of 'osa', 'lv', 'dl', 'hamming', 'lcs', 'qgram', 'cosine', 'jaccard', 'jw', 'soundex'")
  }
  groups <- cutree(clusters, k=k)
  if (!is.null(levels)){
    output <- factor(as.vector(groups), levels=1:length(levels), labels=levels[order(apply(sapply(split(as.data.frame(stringdist::stringdistmatrix(x_na, levels, method=method, useNames = TRUE, ...)), groups), colMeans), 1, which.min))])
  } else{
    output <- groups
    cat("Groups of levels:\n \n")
    print(lapply(split(names(groups), groups), function(x) unique(x)))
  }
  if(length(x) != length(x_na)){
    ind <- which(is.na(x))
    ind <- ind - seq(0, length(ind)-1)
    val <- c(as.character(output), rep(NA, length(ind)))
    final <- c(seq_along(x_na), ind-0.5)
    output <- val[order(final)]
  }
  if(track){
    observations <- which(!(data[,factor_name] %in% output))
    changes <- data.frame(variable=factor_name,
                          observation=observations,
                          original=data[observations, factor_name],
                          new=output[observations],
                          fun="fix.levels",
                          row.names=NULL)
    if(!is.null(changes_old)){
      attr(data, "changes") <- rbind(changes_old, changes)
    } else {
      attr(data, "changes") <- changes
    }
  }
  data[, factor_name] <- output
  invisible(data)
}

#' fix.NA
#'
#' @description Fixes miscoded missing values
#' @param x A data.frame
#' @param na.strings Strings to be considered NA
#' @param track Track changes?
#' @export
#' @examples
#' mydata <- data.frame(prueba = c("", NA, "A", 4, " ", "?", "-", "+"),
#' casa = c("", 1, 2, 3, 4, " ", 6, 7))
#' fix.NA(mydata)
fix.NA <- function(x, na.strings=c("^$", "^ $", "^\\?$", "^-$", "^\\.$", "^NaN$", "^NULL$", "^N/A$"), track=TRUE){
  changes_old <- attr(x, "changes")
  string <- paste(na.strings, collapse="|")
  output <- as.data.frame(lapply(x, function(x) {
    kk <- class(x)
    x <- gsub(string, NA, x)
    tryCatch(eval(parse(text=paste("as.", kk, "(x)", sep=""))), error=function(e) as.character(x))
  }))
  if(track){
    variables <- names(x)[which(sapply(x, function(x) sum(is.na(x))) != sapply(output, function(x) sum(is.na(x))))]
    changes <- do.call(rbind, lapply(variables, function(y){
      observations <- which((!is.na(x[, y]) | is.nan(x[, y])) & is.na(output[, y]))
      data.frame(variable=y,
                 observation=observations,
                 original=x[observations, y],
                 new=output[observations, y],
                 fun="fix.NA",
                 row.names=NULL)
    }))
    if(!is.null(changes_old)){
      attr(output, "changes") <- rbind(changes_old, changes)
    } else {
      attr(output, "changes") <- changes
    }
  }
  return(output)
}

#' fix.concat
#'
#' @description Fixes concatenated values in a variable
#' @param x A data.frame
#' @param varname Variable name
#' @param sep Separator for the different values
#' @param track Track changes?
#' @export
#' @examples
#' mydata <- data.frame(concat=c("a", "b", "a b" , "a b, c", "a; c"),
#' numeric = c(1, 2, 3, 4, 5))
#' fix.concat(mydata, "concat")
fix.concat <- function(x, varname, sep=", |; | ", track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  new_vars <- sapply(unique(unlist(strsplit(x[,varname], sep))), function(y) as.numeric(grepl(y, x[,varname])))
  colnames(new_vars) <- paste(varname, colnames(new_vars), sep="_")
  x <- data.frame(x, new_vars)
  if(!identical(old, x)){
    if(track){
      changes <- data.frame(variable=colnames(new_vars),
                            observation="all",
                            original=NA,
                            new="logical",
                            fun="fix.concat", row.names=NULL)
      if(!is.null(changes_old)){
        attr(x, "changes") <- rbind(changes_old, changes)
      } else {
        attr(x, "changes") <- changes
      }
    }
    return(x)
  } else return(old)
}

#' remove_empty
#'
#' @description Removes empty rows or columns from data.frames
#' @param x A data.frame
#' @param track Track changes?
#' @export
#' @examples
#' mydata <- data.frame(a = c(NA, NA, NA, NA, NA), b = c(1, NA, 3, 4, 5),
#' c=c(NA, NA, NA, NA, NA), d=c(4, NA, 5, 6, 3))
#' remove_empty(mydata)
remove_empty <- function(x, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  empty_rows <- apply(x, 1, function(x) all(is.na(x)))
  empty_cols <- apply(x, 2, function(x) all(is.na(x)))
  x <- x[!empty_rows, !empty_cols]
  if(!identical(old, x)){
    if(track){
      if(sum(empty_cols)>0){
        changes_col <- data.frame(variable=names(old)[!names(old) %in% names(x)],
                                  observation="all",
                                  original=NA,
                                  new="removed",
                                  fun="remove_empty", row.names=NULL)
      } else changes_col <- NULL
      if(sum(empty_rows)>0){
        changes_row <- data.frame(variable="all",
                                  observation=paste(which(empty_rows), "*", sep=""),
                                  original=NA,
                                  new="removed",
                                  fun="remove_empty",
                                  row.names=NULL)
      } else changes_row <- NULL
      changes <- rbind(changes_col, changes_row)
      if(!is.null(changes_old)){
        attr(x, "changes") <- rbind(changes_old, changes)
      } else {
        attr(x, "changes") <- changes
      }
    }
    return(x)
  } else return(old)
}


#' track_changes
#'
#' @description Gets a data.frame with all the changes performed by the different fix functions
#' @param x A data.frame
#' @param subset Logical expression for subsetting the data.frame with the changes
#' @export
#' @examples
#' mydata<-data.frame(Dates1=c("25/06/1983", "25-08/2014", "2001/11/01", "2008-10-01"),
#'                    Dates2=c("01/01/85", "04/04/1982", "07/12-2016", NA),
#'                    Numeric1=rnorm(4))
#' mydata <- fix.dates(mydata)
#' mydata
#' track_changes(mydata)
track_changes <- function(x, subset){
  changes <- attr(x, "changes")
  if(is.null(changes)){
    changes
  } else{
    if(missing(subset)) f <- rep(TRUE, nrow(changes)) else f <- eval(substitute(subset), changes, baseenv())
    changes[f,]
  }
}

#' Restore changes
#'
#' @description Restores original values after using a fix function
#' @param x A data.frame
#' @param var.names Character vector with names of the variables to be restored
#' @export
#' @examples
#' mydata<-data.frame(Dates1=c("25/06/1983", "25-08/2014", "2001/11/01", "2008-10-01"),
#'                    Dates2=c("01/01/85", "04/04/1982", "07/12-2016", NA),
#'                    Numeric1=rnorm(4))
#' mydata <- fix.dates(mydata)
#' mydata
#' mydata <- restore_changes(mydata, "Dates1")
#' mydata
restore_changes <- function(x, var.names){
  changes <- track_changes(x)
  if(!all(changes$fun[changes$variable == var.names] %in% c("fix.numerics", "fix.factors", "fix.dates", "fix.levels"))) stop("Variable(s) changed by ", paste(changes$fun[changes$variable == var.names], sep=", "), ". Currently, restore_changes() only works for changes performed by fix.numerics, fix.factors, fix.dates and fix.levels functions", sep="")
  x[, var.names] <- lapply(var.names, function(y){
    changes.y <- changes[changes$variable == y,]
    old.class <- changes.y$original[changes.y$observation == "all"][1]
    if(!is.na(old.class)) class(x[, y]) <- old.class else x[, y] <- as.character(x[, y])
    x[, y][as.numeric(changes.y$observation[changes.y$observation != "all"])] <- changes.y$original[changes.y$observation != "all"]
    x[, y]
  })
  changes <- changes[!changes$variable %in% var.names, ]
  attr(x, "changes") <- changes
  x
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
    if(class(x) %in% c("character", "factor")){
      length(unique(x))
    }
    else if(is.numeric(x)){
      paste(round(range(x, na.rm=TRUE),2), collapse="-")
    }
    else {
      ""
    }
  }
  ), ")", sep="")
  blank <- rep("", length=length(class))
  output <- rbind(as.matrix(head(x[,which], n)), rep("", ncol(x[,which])), class, range, blank, blank)
  rownames(output)[(n+4):(n+5)]<-""
  cat("Data frame with ", nrow(x), " rows (showing ", length(which), " of ", ncol(x), " variables) \n \n")
  print(output, quote = FALSE)
}

#' Nice names
#'
#' @description Changes names of a data frame to ease work with them
#' @param x A data.frame
#' @param track Track changes?
#' @export
#' @examples
#' d <- data.frame('Variable 1'=NA, '% Response'=NA, ' Variable     3'=NA,check.names=FALSE)
#' names(d)
#' names(nice_names(d))
nice_names <- function(x, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  old_names <- names(x)
  new_names <- gsub("x_","",gsub("_$", "",tolower(gsub("[_]+", "_",gsub("[.]+", "_",make.names(
    gsub("^[ ]+", "",gsub("%", "percent",gsub("\"", "",gsub("'", "",gsub("\u00BA", "", old_names)))))))))))
  dupe_count <- sapply(1:length(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count >
                                                 1], dupe_count[dupe_count > 1], sep = "_")
  new_names <- iconv(new_names, to = "ASCII//TRANSLIT")
  x <- stats::setNames(x, new_names)
  if(!identical(old_names, new_names)){
    if(track){
      changes <- data.frame(variable=new_names[old_names != new_names],
                            observation="varname",
                            original=old_names[old_names != new_names],
                            new=new_names[old_names != new_names],
                            fun="nice_names", row.names=NULL)
      if(!is.null(changes_old)){
        attr(x, "changes") <- rbind(changes_old, changes)
      } else {
        attr(x, "changes") <- changes
      }
    }
    return(x)
  } else return(old)
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

