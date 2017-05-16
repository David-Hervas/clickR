#' Detailed summary of the data
#'
#' @description Creates a detailed summary of the data
#' @param x A data.frame
#' @param z Number of decimal places
#' @param graph If TRUE displays a graph of variable clustering
#' @param ignore.na If TRUE NA values will not count for relative frequencies calculations
#' @param by Factor variable definining groups for the summary
#' @return Summary of the data
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom graphics axis boxplot image mtext par plot
#' @importFrom stats AIC confint cutree density dist family hclust median na.omit quantile rect.hclust sd var
#' @importFrom utils write.csv2
#' @export
#' @examples
#' descriptivo(iris)
#' descriptivo(iris, by="Species")
descriptivo<-function(x, z=3, graph=ifelse(length(data.frame(x))>20, FALSE, TRUE), ignore.na=T, by=NULL){
  summary1<-summary2<-NULL
  #Data.frame
  if(is.data.frame(x)==F){
    x<-data.frame(x)}
  x<-x[,!sapply(x, function(x) sum(is.na(x))/length(x))==1]
  if(is.data.frame(x)==F){
    x<-data.frame(x)}
  if(!is.null(by))
  {
    if (by %in% names(x))
    {
      pos_by <- match(by,names(x))
      by_v <- eval(parse(text=paste("x$",by,sep="")))
      x_sin <- data.frame(x[,-pos_by])
      names(x_sin) <- names(x)[-pos_by]
      if (length(x_sin)==0)
      {
        descriptivo(x,z,graph,ignore.na,by=NULL)
        stop("Only one variable in the data. Can't be used as grouping variable")
      }
    }
    else{
      pos_by<-NULL
      by_v <- eval(parse(text=by))
      x_sin <- x
    }

    if (length(by_v)!=dim(x_sin)[1] | is.numeric(by_v))
    {
      descriptivo(x=x,z=z,graph=graph,ignore.na=ignore.na,by=NULL)
      warning(gettextf("Variable %s does not have the same number of observations than the data or is not a factor.
                       Summary without grouping.", by))
    }
    else
    {
      x_sin <- x_sin[!is.na(by_v),]
      by_v <- by_v[!is.na(by_v)]
      by_v <- factor(by_v)
      x_sin <- data.frame(x_sin)
      if(!is.null(pos_by)) names(x_sin) <- names(x)[-pos_by]
      niveles <- levels(by_v)
      cat("Summary by ", by, ":", sep="")
      cat("\n")
      cat("-------------------------------")
      cat("\n")
      for (i in 1:length(niveles))
      {
        x_g <- x_sin[by_v==niveles[i],]
        cat("Level ", by, ": ", niveles[i], sep="")
        cat("\n")
        descriptivo(x=x_g,z=z,graph=F,ignore.na=ignore.na,by=NULL)
        cat("\n")
        cat("-------------------------------")
        cat("\n")
      }
    }
    }
  else
  {
    #Splitter (Splits data.frame: Numeric and categorical part)
    nums <- sapply(x, is.numeric)

    #Numeric (Apply descriptives for numeric data)
    kurtosis <- function(x) {
      m4 <- mean((x-mean(x, na.rm=T))^4, na.rm=T)
      kurt <- m4/(sd(x, na.rm=T)^4)-3
      kurt
    }
    skewness <-  function(x) {
      m3 <- mean((x-mean(x, na.rm=T))^3, na.rm=T)
      skew <- m3/(sd(x, na.rm=T)^3)
      skew
    }
    moda_cont <- function(x) {
      if(length(na.omit(x))>1){
        modas2 <- sum(diff(diff(density(x, adjust=2, na.rm=T)$y)>=0)<0)
        modas1 <- sum(diff(diff(density(x, adjust=1, na.rm=T)$y)>=0)<0)
        if(modas1!=modas2 & modas2==1)
          return(modas2+0.5)
        else
          return(modas2)
      }
      else return(0)
    }
    resumen<-function(y){
      resumen1<-round(c(min(y, na.rm=T), quantile(y, probs=0.25, na.rm=T), median(y, na.rm=T), quantile(y, probs=0.75, na.rm=T), max(y, na.rm=T), mean(y, na.rm=T), sd(y, na.rm=T), kurtosis(y), skewness(y), moda_cont(y), sum(is.na(y))),z)
      t(resumen1)
    }

    #Categorical (Apply descriptives for categorical data)
    moda<-function(x){names(sort(-table(x)))[1]}
    antimoda<-function(x){names(sort(table(x)))[1]}
    prop_may<-function(x) {sort(-table(x))[1]/-(length(x)-ignore.na*sum(is.na(x)))}
    prop_min<-function(x){sort(table(x))[1]/(length(x)-ignore.na*sum(is.na(x)))}
    niveles<-function(x) {length(levels(x))}
    resumen2<-function(w){
      resumen2<-c(length(table(w)), ifelse(nchar(paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/"))<18, paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/"), ifelse(nchar(gsub("[aeiouAEIOU]", "", paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/")))>25, paste(strtrim(gsub("[aeiouAEIOU]", "", paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/")), 23), "...", sep=""), gsub("[aeiouAEIOU]", "", paste(na.omit(names(sort(-table(w)))[1:5]), collapse="/")))), moda(w), round(prop_may(w),z), antimoda(w), round(prop_min(w),z), sum(is.na(w)))
      t(resumen2)
    }

    #Results
    cat(paste("Data frame with", dim(x)[1], "observations and", dim(x)[2], "variables."))
    cat("\n")
    cat("\n")
    if("TRUE" %in% names(table(nums))){
      cat("Numeric variables (", sum(nums), ")", sep="")
      cat("\n")
      assign("summary1", apply(x[nums==T], 2, resumen), inherits=TRUE)
      rownames(summary1)<-c("Min.", "1st Q.", "Median", "3rd Q.", "Max.", "Mean", "SD", "Kurtosis", "Asymmetry", "N. Modes", "NAs")
      if (sum(summary1[10,]==1.5)==0)
        print(summary1)
      else{
        summary1[c(-10,-11),] <- format(round(summary1[c(-10,-11),],z),nsmall=z)
        summary1[10,][summary1[10,]==1.5] <- "1*"
        print(summary1,quote=F,right=T)
      }
    }
    if("FALSE" %in% names(table(nums))){
      assign("summary2", apply(x[nums==F], 2, resumen2), inherits = TRUE)
      rownames(summary2)<-c("N. Classes", "Classes", "Mode", "Prop. mode", "Anti-mode", "Prop. Anti-mode", "NAs")
      cat("\n")
      cat("Categorical variables (", dim(x)[2]-sum(nums), ")", sep="")
      cat("\n")
      print(summary2, quote=FALSE)
    }
    if(length(x)>2 & graph==T){
      sd2 <- function(x) var(if(is.numeric(x)) x else as.numeric(as.factor(x)), na.rm=T)
      sd_0 <- sapply(x,sd2)==0
      X.quanti<-NULL
      X.quali<-NULL

      if(length(x[nums==T & !sd_0])>0){
        X.quanti<-x[nums==T & !sd_0]}
      if(length(x[nums==F & !sd_0])>0){
        X.quali<-x[nums==F & !sd_0]}

      plot(ClustOfVar::hclustvar(X.quanti, X.quali), which=1, main="Clustering of variables")

      if(sum(sd_0)>0)
        warning(gettextf("Variables %s have been omited since they are constant",
                         paste(names(x)[sd_0], collapse=", ")))
    }
  }
  return(invisible(list(Numerical=summary1, Categorical=summary2)))
}

#' Mine plot
#'
#' @description Creates a heatmap-like plot for exploring the data
#' @param x A data.frame
#' @param what A logical expresion that will be depicted in the plot
#' @param spacing Numerical separation between lines at the y-axis
#' @param sort If TRUE, variables are sorted according to their results
#' @param list If TRUE, creates a vector with the results
#' @param ... further arguments passed to order()
#' @export
#' @examples
#' mine.plot(airquality)   #Displays missing data
#' mine.plot(airquality, what="x>mean(x)+2*sd(x) | x<mean(x)-2*sd(x)")   #Shows extreme values
mine.plot <- function(x, what="is.na(x)", spacing=5, sort=F, list=FALSE, ...){
  eval(parse(text=paste("is.it<-function(x)", what)))
  x<-as.data.frame(x)
  if(sort){
    orden <- order(sapply(x, function(x) sum(is.it(x))), ...)
    x <- x[,orden]
  }
  old.warn <- options(warn=-1)
  pad<- ceiling(dim(x)[2]/30)
  old.par <- par(mar=c(8, 4.5, 6, 4))
  image(t(sapply(x, function(x) is.it(x))), xaxt="n", yaxt="n", col=colorRampPalette(c("lightcyan4", "darkred"))(2))
  axis(1, at=seq(0, 1, length=dim(x)[2]), labels=names(x), las=2, lwd=0, cex.axis=0.8)
  axis(2, at=seq(0, dim(x)[1], by=spacing)/dim(x)[1], labels=seq(0, dim(x)[1], by=spacing), las=1, cex.axis=0.6)
  for(i in 1:pad){
    axis(3, at=seq(0, 1, length=dim(x)[2])[seq(0+i, dim(x)[2], by=pad)],
         labels=sapply(x, function(x) round(100*sum(is.it(x))/length(x)))[seq(0+i, dim(x)[2], by=pad)], cex.axis=0.6, lwd=0, line=-1+i/2)
  }
  mtext(paste("%", what), 3, line=max(pad/1.5, 2.5), cex=1.2)
  options(old.warn)
  if(list){
    return(sapply(x, function(x) round(100*sum(is.it(x))/length(x))))
  }
  par(old.par)
}

#' is.it
#'
#' @description Internal function for mine.plot
#' @param x logical expression
is.it <- function(x) is.na(x)


#' Improved boxplot
#'
#' @description Creates an improved boxplot with individual data points
#' @param formula Formula for the boxplot
#' @param boxwex Width of the boxes
#' @param ... further arguments passed to beeswarm()
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param add.rownames Logical for adding rownames to the table
#' @param ... further arguments passed to make_table()
#' @export
#' @examples
#' report(iris)
#' (reporTable<-report(iris, by="Species"))
#' class(reporTable)
report.data.frame<-function(x, by=NULL, file=NULL, type="word",
                            font=ifelse(Sys.info()["sysname"] == "Windows", "Arial",
                                        "Helvetica")[[1]], pointsize=11,
                            add.rownames=FALSE, ...){
  if(is.data.frame(x)==F){
    x<-data.frame(x)}
  x<-x[,!sapply(x, function(x) sum(is.na(x))/length(x))==1, drop=FALSE]
  x[sapply(x, is.factor) & sapply(x, function(x) !all(levels(x) %in% unique(na.omit(x))))]<-lapply(x[sapply(x, is.factor) & sapply(x, function(x) !all(levels(x) %in% unique(na.omit(x))))], factor)
  if(length(by)>1){
    x.int <- data.frame(x, by=interaction(x[, match(unlist(by), names(x))]))
    report(x.int[,-match(unlist(by), names(x.int))], by="by")
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
    estruct[1, -1]<-"Mean (SD)"
    estruct[2,-1]<-"Median (1st, 3rd Q.)"
    cont<-character(2*length(x[nums==T]))
    cont[seq(1,length(cont), 2)]<-colnames(x[,nums==T, drop=FALSE])
    if(ncol(x[,nums==T, drop=FALSE])>1){
      A<-matrixPaste(sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(mean(x, na.rm=TRUE),2)))), function(x) t(x)), " (",
                     sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(sd(x, na.rm=TRUE),2)))), function(x) t(x)),")", sep=rep("", 3))

      B<-matrixPaste(sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(median(x, na.rm=TRUE),2)))), function(x) t(x)),
                     " (",
                     sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(quantile(x, 0.25, na.rm=TRUE),2)))), function(x) t(x)),
                     ", ",
                     sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(quantile(x, 0.75, na.rm=TRUE),2)))), function(x) t(x)),
                     ")", sep=rep("", 5))
    }
    else {
      A<-paste(sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(mean(x, na.rm=TRUE),2)))), function(x) t(x)), " (",
               sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(sd(x, na.rm=TRUE),2)))), function(x) t(x)),")", sep=rep(""))
      B<-paste(sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(median(x, na.rm=TRUE),2)))), function(x) t(x)),
               " (",
               sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(quantile(x, 0.25, na.rm=TRUE),2)))), function(x) t(x)),
               ", ",
               sapply(by(x, by_v, function(x) sapply(x[nums==T],function(x) as.character(round(quantile(x, 0.75, na.rm=TRUE),2)))), function(x) t(x)),
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
                   sapply(by(x[nums==F], by_v, function(x) sapply(x, function(x) as.character(round(100*(table(x)/sum(table(x))),1)))), function(x) unlist(x)),"%)", sep=rep("", 3))
    cats[-(rev(rev(cumsum(c(1,pos)))[-1])+rev(rev((0:(dim(x[nums==F])[2])))[-1])),-1]<-C
  }

  #Matrix binding
  output<-rbind(estruct, AB, cats)
  colnames(output)<-c("Variable", paste(by, levels(by_v), sep=" ", "n =", as.vector(table(by_v))))

  if(!is.null(file)) make_table(output, file, type, font, pointsize, add.rownames)
  return(print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=FALSE, right=FALSE))
  }
}

#' Multiple tapply
#'
#' @description Modification of the tapply function to use with data.frames
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
#' @export
#' @examples
#' report(mtcars)
#' report(fix.factors(mtcars))
fix.factors<-function(x, k=5, drop=TRUE){
  x[, (sapply(x, function(x) is.numeric(x) & length(unique(x))<=k)) | (sapply(x, function(x) is.factor(x)))]<-lapply(x[, sapply(x, function(x) is.numeric(x) & length(unique(x))<=k) | (sapply(x, function(x) is.factor(x))), drop=FALSE], function(x) if(drop) factor(iconv(droplevels(as.factor(gsub("^ *|(?<= ) | *$", "", tolower(as.character(x)), perl=TRUE))), to="ASCII//TRANSLIT")) else factor(x))
  return(x)
}

#' Fix numeric data
#'
#' @description Fixes numeric data
#' @param x A data.frame
#' @param k Minimum number of different values to be considered numerical
#' @param max.NA Maximum allowed proportion of NA values created by coercion
#' @param info Add generated missing values an excluded variable information as attributes
#' @export
#' @examples
#' mydata<-data.frame(Numeric1=c(7.8, 9.2, 5.4, 3.3, "6,8", "3..3"),
#'                    Numeric2=c(3.1, 1.2, "3.s4", "a48,s5", 7, "6,,4"))
#' report(mydata)
#' report(fix.numerics(mydata, k=5))
fix.numerics<-function(x, k=8, max.NA=0.2, info=TRUE){
  x.old<-x
  previous.NA<- sapply(x, function(x) sum(is.na(x)))
  x[, apply(sapply(x, function(x) grepl("[0-9]", as.character(x))), 2, any) & sapply(x, function(x) !is.numeric(x)) & sapply(x, function(x) length(unique(x))>=k)] <- sapply(x[, apply(sapply(x, function(x) grepl("[0-9]", as.character(x))), 2, any) & sapply(x, function(x) !is.numeric(x))  & sapply(x, function(x) length(unique(x))>=k), drop=FALSE], function(x) numeros(x))
  final.NA<-sapply(x, function(x) sum(is.na(x)))-previous.NA
  x[,(final.NA-previous.NA) > nrow(x)*max.NA]<-x.old[,(final.NA-previous.NA) > nrow(x)*max.NA]
  print(paste(sum(sapply(x, function(x) sum(is.na(x)))-previous.NA), "new missing values generated"))
  print(paste(sum((final.NA-previous.NA) > nrow(x)*max.NA), "variables excluded following max.NA criterion"))
  if(info){
    attr(x, "missing") <- (sapply(x, function(x) sum(is.na(x)))-previous.NA)
    attr(x, "excluded") <- (final.NA-previous.NA) > nrow(x)*max.NA
  }
  return(x)
}


#' Fix dates
#'
#' @description Fixes dates
#' @param x A data.frame
#' @param cent Fixes century
#' @export
#' @examples
#' mydata<-data.frame(Dates1=c("25/06/1983", "25-08/2014", "2001/11/01", "2008-10-01"),
#'                    Dates2=c("01/01/85", "04/04/1982", "07/12-2016", NA),
#'                    Numeric1=rnorm(4))
#' fix.dates(mydata)
fix.dates <- function (x, cent = "19"){
  previous.NA <- sapply(x, function(x) sum(is.na(x)))
  x[, apply(sapply(x, function(x) grepl("(-{1}|/{1}).{1,4}(-{1}|/{1})", as.character(x))), 2, any)] <- lapply(x[, apply(sapply(x,
      function(x) grepl("(-{1}|/{1}).{1,4}(-{1}|/{1})", as.character(x))), 2, any), drop = FALSE],
      function(x) as.Date(gsub("(?<![0-9])0{2}+", cent, perl = TRUE, as.Date(sapply(strsplit(gsub("/", "-", as.character(x)), "-"),
      function(x) if (is.na(x[1]) | is.na(as.numeric(x[1]))) NA else if (!as.numeric(x[1]) > 31) paste(rev(x), collapse = "-") else paste(x, collapse = "-"))))))
  final.NA <- sum(sapply(x, function(x) sum(is.na(x))) - previous.NA)
  print(paste(final.NA, "new missing values generated"))
  return(x)
}


#' Fix levels
#'
#' @description Fixes levels of a factor
#' @param x A factor vector
#' @param levels Optional vector with the levels names
#' @param plot Optional: Plot cluster dendrogram?
#' @param k Number of levels for clustering
#' @export
#' @examples
#' factor1<-factor(c("Control", "Treatment", "Tretament", "Tratment", "treatment",
#' "teatment", "contrl", "cntrol", "CONTol", "not available", "na"))
#' fix.levels(factor1, k=4, plot=TRUE)   #Chose k to select matching levels
#' fix.levels(factor1, levels=c("Control", "Treatment"), k=4)
fix.levels<-function(x, levels=NULL, plot=FALSE, k=ifelse(!is.null(levels), length(levels), 2)){
  listado<-unique(unlist(strsplit(tolower(as.character(x)), "")))
  simil<-sapply(strsplit(tolower(as.character(x)), ""), function(x) listado %in% x)
  rownames(simil)<-listado
  colnames(simil)<-as.character(x)
  clusters<-hclust(dist(t(simil), method="binary"))
  if(plot) {
    clusplot<-hclust(dist(unique(t(simil)), method="binary"))
    plot(clusplot)
    rect.hclust(clusplot, k=k, border="red")
  }
  groups <- cutree(clusters, k=k)
  if (!is.null(levels)){
    p<-1
    for(i in groups[which(names(groups) %in% levels)]){
      x[x %in% names(groups)[groups==i]]<-names(groups[which(names(groups) %in% levels)])[p]
      p<-p+1
    }
    x[! x %in% levels]<-NA
    return(droplevels(factor(x)))
  } else{
    return(groups)
  }
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
      paste(range(x), collapse="-")
    }
    else {
      ""
    }
  }
  ), ")", sep="")
  cat("Data frame with ", nrow(x), " rows (showing ", length(which), " of ", ncol(x), " variables) \n \n")
  print(rbind(as.matrix(head(x[,which], n)), rep("", ncol(x[,which])), class, range), quote = FALSE)
}

#' Nice names
#'
#' @description Changes names of a data frame to ease work with them
#' @param dat A data.frame
#' @export
#' @examples
#' d <- data.frame('Variable 1'=NA, '% Response'=NA, ' Variable     3'=NA,check.names=FALSE)
#' names(d)
#' names(nice_names(d))
nice_names<-function (dat){
  old_names <- names(dat)
  new_names <- gsub("x_","",gsub("_$", "",tolower(gsub("[_]+", "_",gsub("[.]+", "_",make.names(
    gsub("^[ ]+", "",gsub("%", "percent",gsub("\"", "",gsub("'", "",gsub("º", "", old_names)))))))))))
  dupe_count <- sapply(1:length(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count >
                                                 1], dupe_count[dupe_count > 1], sep = "_")
  new_names <- iconv(new_names, to = "ASCII//TRANSLIT")
  stats::setNames(dat, new_names)
}
