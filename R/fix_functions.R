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
        changes_old$variable <- changes$new[match(changes_old$variable, changes$original)]
        attr(x, "changes") <- rbind(changes_old, changes)
      } else {
        attr(x, "changes") <- changes
      }
    }
    return(x)
  } else return(old)
}

#' Fix factors imported as numerics
#'
#' @description Fixes factors imported as numerics. It is usual in some fields to encode
#' factor variables as integers. This function detects such variables and transforms
#' them into factors. When \code{drop=TRUE} (by default) it detects multiple versions
#' of the same levels due to different capitalization, whitespaces or non-ASCII characters.
#' @param x A data.frame
#' @param k Maximum number of different numeric values to be converted to factor
#' @param drop Drop similar levels?
#' @param track Keep track of changes?
#' @export
#' @examples
#' # mtcars data has all variables encoded as numeric, even the factor variables.
#' report(mtcars)
#' # After using fix.factors, factor variables are recognized as such.
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
#' @description Fixes numeric data. In may cases, numeric data are not recognized by R
#' because there are data inconsistencies (wrong decimal separator, whitespaces, typos,
#' thousand separator, etc.). \code{fix.numerics} detects and corrects these variables,
#' making them numeric again.
#' @param x A data.frame
#' @param k Minimum number of different values a variable has to have to be considered numerical
#' @param max.NA Maximum allowed proportion of NA values created by coercion. If the
#' coercion to numeric creates more NA values than those specified in \code{max.NA}, then all
#' changes will be reverted and the variable will remain unchanged.
#' @param track Keep track of changes?
#' @export
#' @examples
#' mydata<-data.frame(Numeric1=c(7.8, 9.2, "5.4e+2", 3.3, "6,8", "3..3"),
#'                    Numeric2=c(3.1, 1.2, "3.4s", "48,500.04 $", 7, "$  6.4"))
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
  x[, candidate_variables & !sci_notation_variables] <- lapply(x[, candidate_variables & !sci_notation_variables, drop=FALSE], function(x) numeros(x))
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
        observations <- rownames(x)[which(!(old[, y] %in% x[, y]))]
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
#' @description Fixes dates. Dates can be recorded in numerous formats depending on the
#' country, the traditions and the field of knowledge. \code{fix.dates} tries to detect
#' all possible date formats and transforms all of them in the ISO standard favored by
#' R (yyyy-mm-dd).
#' @param x A data.frame
#' @param max.NA Maximum allowed proportion of NA values created by coercion. If the
#' coercion to date creates more NA values than those specified in \code{max.NA}, then all
#' changes will be reverted and the variable will remain unchanged.
#' @param min.obs Minimum number of non-NA observations allowed per variable. If the variable
#' has fewer non-NA observations, then it will be ignored by \code{fix.dates}.
#' @param use.probs When there are multiple date formats in the same column, there can
#' be ambiguities. For example, 04-06-2015 can be interpreted as 2015-06-04 or as 2015-04-06.
#' If \code{use.probs=TRUE}, ambiguities will be solved by assigning to the most frequent
#' date format in the column.
#' @param track Track changes?
#' @export
#' @examples
#' mydata<-data.frame(Dates1=c("25/06/1983", "25-08/2014", "2001/11/01", "2008-10-01"),
#'                    Dates2=c("01/01/85", "04/04/1982", "07/12-2016", "September 24, 2020"),
#'                    Numeric1=rnorm(4))
#' fix.dates(mydata)
fix.dates <- function (x, max.NA=0.8, min.obs=nrow(x)*0.05, use.probs=TRUE, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  x<-kill.factors(x)
  x.old<-x
  previous.NA <- sapply(x, function(x) sum(is.na(x)))
  previous.minobs <- sum(sapply(x, function(x) sum(!is.na(x))<min.obs))
  candidate_variables <- apply(sapply(x, function(x) grepl("(-{1}|/{1}).{1,4}(-{1}|/{1})", as.character(x))), 2, any) &
    sapply(x, function(x) class(x)!="Date")
  x[, candidate_variables] <- lapply(x[, candidate_variables, drop = FALSE], function(x) fxd(x, use.probs=use.probs))
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
      observations <- rownames(x)[which(!(as.character(old[, y]) %in% as.character(x[, y])))]
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

#' Internal function for dates with text
#'
#' @description Function to transform text into dates
#' @param date A date
#' @param format Format of the date
text_date <- function(date, format="%d/%Y %b"){
  translate <- data.frame(spanish=c("ene","feb","mar","abr","may","jun","jul","ago",
                                    "sep","oct","nov","dic","enero","febrero","marzo",
                                    "abril","mayo","junio","julio","agosto","septiembre",
                                    "octubre","noviembre","diciembre"),
                          english=c(tolower(month.abb),
                                    tolower(month.name)))
  month_abb <- tolower(paste(month.abb, collapse="|"))
  date <- tolower(date)
  month_t <- sapply(translate$spanish, function(x) grepl(x, date))
  if(sum(month_t)>0) date <- gsub(names(month_t[month_t])[sum(month_t)], translate$english[month_t][sum(month_t)], date)
  x <- gregexpr("[0-9]+", date)
  y <- gregexpr(month_abb, date)
  day_year <- unlist(regmatches(date, x))
  day_year <- day_year[order(as.numeric(day_year))]
  month <- unlist(regmatches(date, y))
  as.Date(paste(paste(day_year, collapse="/"), month), format=format)
}

#' Internal function to fix.dates
#'
#' @description Function to format dates
#' @param d A character vector
#' @param use.probs Solve ambiguities by similarity to the most frequent formats
fxd <- function(d, use.probs=TRUE){
  formats <- c("%d-%m-%Y", "%d-%m-%y", "%Y-%m-%d", "%m-%d-%Y", "%m-%d-%y", "%d-%b-%Y", "%d-%B-%Y", "%d-%b-%y", "%d-%B-%y",
               "%d%m%Y", "%d%m%y", "%Y%m%d", "%m%d%Y", "%m%d%y", "%d%b%Y", "%d%B%Y", "%d%b%y", "%d%B%y")
  Sys.setlocale("LC_TIME", "C")
  prueba <- lapply(formats, function(x) as.Date(tolower(gsub("--", "-", gsub("[[:punct:]]", "-", gsub("[[:space:]]+", "", d)))), format=x))
  text_dates <- do.call(c, lapply(d, text_date))
  text_dates2 <- do.call(c, lapply(d, function(x){
    if(any(as.numeric(unlist(regmatches(x, gregexpr("[0-9]+", x)))) >= 32)){
      text_date(x, format="%d/%y %b")
    } else NA
  }))
  prueba[[19]] <- text_dates
  prueba[[20]] <- text_dates2
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
  final_dates[(abs(years - median_year) %>NA% abs(years-100 - median_year)) & nchar(d)<=8] <- do.call(c, lapply(final_dates[(abs(years - median_year) %>NA% abs(years-100 - median_year)) & nchar(d)<=8], function(x) tryCatch(seq(x, length=2, by="-100 years")[2], error=function(e) NA)))
  final_dates[(abs(years - median_year) %>NA% abs(years+100 - median_year)) & nchar(d)<=8] <- do.call(c, lapply(final_dates[(abs(years - median_year) %>NA% abs(years+100 - median_year)) & nchar(d)<=8], function(x) tryCatch(seq(x, length=2, by="100 years")[2], error=function(e) NA)))
  Sys.setlocale("LC_TIME", "")
  return(final_dates)
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
      plot(clusplot, ann=FALSE)
      mtext("Height", 2, line=2.5)
      rect.hclust(clusplot, k=k, border="red")
    }
  } else{
    stop("Method should be one of 'osa', 'lv', 'dl', 'hamming', 'lcs', 'qgram', 'cosine', 'jaccard', 'jw', 'soundex'")
  }
  groups <- cutree(clusters, k=k)
  if (!is.null(levels)){
    if(length(levels) == 1 && levels == "auto") levels <- sapply(split(names(groups), groups), function(x) unique(x)[1])
    output <- factor(as.vector(groups), levels=1:length(levels), labels=levels[order(apply(sapply(split(as.data.frame(stringdist::stringdistmatrix(tolower(x_na), tolower(levels), method=method, useNames = TRUE, ...)), groups), colMeans), 1, which.min))])
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
    observations <- rownames(data)[which(!(data[,factor_name] %in% output))]
    changes <- data.frame(variable=factor_name,
                          observation=observations,
                          original=data[observations, factor_name],
                          new=output[which(!(data[,factor_name] %in% output))],
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
      observations <- rownames(output)[which((!is.na(x[, y]) | is.nan(x[, y])) & is.na(output[, y]))]
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
                                  observation=rownames(old)[empty_rows],
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

#' fix.all
#'
#' @description Tries to fix all problems in the data.frame
#' @param x A data.frame
#' @param track Track changes?
#' @export
fix.all <- function(x, track=TRUE){
  x <- fix.numerics(
    fix.factors(
      fix.dates(
        remove_empty(
          fix.NA(
            nice_names(x,
                       track=track),
            track=track),
          track=track),
        track=track),
      track=track),
    track=track)
  x
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
    attr(changes, "data") <- as.character(match.call()$x)
    if(missing(subset)) f <- rep(TRUE, nrow(changes)) else f <- eval(substitute(subset), changes, baseenv())
    changes[f,]
  }
}

#' Restore changes
#'
#' @description Restores original values after using a fix function
#' @param tracking A data.frame generated by track_changes() function
#' @export
#' @examples
#' mydata<-data.frame(Dates1=c("25/06/1983", "25-08/2014", "2001/11/01", "2008-10-01"),
#'                    Dates2=c("01/01/85", "04/04/1982", "07/12-2016", NA),
#'                    Numeric1=rnorm(4))
#' mydata <- fix.dates(mydata)
#' mydata
#' tracking <- track_changes(mydata)
#' mydata_r <- restore_changes(tracking)
#' mydata_r
restore_changes <- function(tracking){
  data <- get(attr(tracking, "data"))
  old_changes <- attr(data, "changes")
  varnames <- tracking[tracking$fun == "nice_names",]
  create_rows <- unique(tracking$observation[!tracking$observation %in% rownames(data) & !tracking$observation %in% c("varname", "all")])
  create_vars <- unique(tracking$variable[!tracking$variable %in% names(data) & !tracking$variable %in% "all"])
  if(length(create_rows)>0){
    data <- rbind(data, setNames(data.frame(matrix(NA, nrow=length(create_rows), ncol=ncol(data))), names(data)))
    rownames(data)[(nrow(data)-(length(create_rows)-1)):nrow(data)] <- create_rows
    data <- data[order(rownames(data)),]
  }
  if(length(create_vars)>0){
    data <- cbind(data, setNames(data.frame(matrix(NA, nrow=nrow(data), ncol=length(create_vars))), create_vars))
    warning("Cannot recover previous position of deleted variables. Appending at the end of the data.frame")
  }
  if(any(tracking$fun == "fix.concat")) data <- data[,!names(data) %in% tracking$variable[tracking$fun == "fix.concat"]]
  trackingf <- tracking[!tracking$fun %in% c("nice_names", "remove_empty", "fix.concat"),]
  variables <- unique(trackingf$variable)
  data[, variables] <- lapply(variables, function(y){
    changes.y <- trackingf[trackingf$variable == y,]
    old.class <- changes.y$original[changes.y$observation == "all"][1]
    data[, y] <- as.character(data[, y])
    if(!is.na(old.class)) class(data[, y]) <- old.class
    data[changes.y$observation[changes.y$observation != "all"], y] <- changes.y$original[changes.y$observation != "all"]
    data[, y]
  })
  if(nrow(varnames)>0){
    names(data)[names(data) %in% varnames$variable] <- na.omit(varnames$original[match(names(data), varnames$new)])
    old_changes$variable[old_changes$variable != "all"  & !is.na(old_changes$variable)] <- old_changes$original[old_changes$fun == "nice_names"][match(old_changes$variable[old_changes$variable!="all"  & !is.na(old_changes$variable)], old_changes$new[old_changes$fun == "nice_names"])]
    old_changes <- old_changes[!(old_changes$variable %in% varnames$original & old_changes$fun == "remove_empty"),]
  }
  changes <- old_changes[! apply(old_changes[,-1], 1, function(x) paste(x, collapse="")) %in% apply(tracking[,-1], 1, function(x) paste(x, collapse="")), ]
  changes <- changes[!changes$observation %in% create_rows & !changes$variable %in% create_vars,]
  attr(data, "changes") <- changes
  data
}
