#' Nice names
#'
#' @description Changes names of a data frame to ease work with them
#' @param x A data.frame
#' @param select Numeric vector with the positions (all by default) to be affected by the function
#' @param tolower Set all names to lower case?
#' @param track Track changes?
#' @return The input data.frame \code{x} with the fixed names
#' @export
#' @examples
#' d <- data.frame('Variable 1'=NA, '% Response'=NA, ' Variable     3'=NA,check.names=FALSE)
#' names(d)
#' names(nice_names(d))
nice_names <- function(x, select=1:ncol(x), tolower=TRUE, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  old_names <- names(x)
  new_names <- gsub("x_|X_","",gsub("_$", "", gsub("[_]+", "_",gsub("[.]+", "_",make.names(
    iconv(gsub("^[ ]+", "",gsub("%", "percent",gsub("\"", "",gsub("'", "",gsub("\u00BA", "", old_names[select]))))), to="ASCII//TRANSLIT", sub="byte"))))))
  if(tolower) new_names <- tolower(new_names)
  dupe_count <- sapply(1:length(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count >
                                                 1], dupe_count[dupe_count > 1], sep = "_")
  names(x)[select] <- new_names
  new_names <- names(x)
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
#' @param select Numeric vector with the positions (all by default) to be affected by the function
#' @param drop Drop similar levels?
#' @param track Keep track of changes?
#' @export
#' @examples
#' # mtcars data has all variables encoded as numeric, even the factor variables.
#' descriptive(mtcars)
#' # After using fix_factors, factor variables are recognized as such.
#' descriptive(fix_factors(mtcars))
fix_factors<-function(x, k=5, select=1:ncol(x), drop=TRUE, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  candidate_variables <- (sapply(x, function(x) (is.numeric(x) |
                                                   is.character(x)) &
                                   length(unique(x))<=k)) |
    (sapply(x, function(x) is.factor(x)))
  candidate_variables[-select] <- FALSE
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
                            fun="fix_factors", row.names=NULL)
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
#' @description Fixes numeric data. In many cases, numeric data are not recognized by R
#' because there are data inconsistencies (wrong decimal separator, whitespaces, typos,
#' thousand separator, etc.). \code{fix_numerics} detects and corrects these variables,
#' making them numeric again.
#' @param x A data.frame
#' @param k Minimum number of different values a variable has to have to be considered numerical
#' @param max.NA Maximum allowed proportion of NA values created by coercion. If the
#' coercion to numeric creates more NA values than those specified in \code{max.NA}, then all
#' changes will be reverted and the variable will remain unchanged.
#' @param select Numeric vector with the positions (all by default) to be affected by the function
#' @param track Keep track of changes?
#' @export
#' @examples
#' mydata<-data.frame(Numeric1=c(7.8, 9.2, "5.4e+2", 3.3, "6,8", "3..3"),
#'                    Numeric2=c(3.1, 1.2, "3.4s", "48,500.04 $", 7, "$  6.4"))
#' descriptive(mydata)
#' descriptive(fix_numerics(mydata, k=5))
fix_numerics <- function(x, k=8, max.NA=0.2, select=1:ncol(x), track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  previous.NA<- sapply(x, function(x) sum(is.na(x)))
  candidate_variables <- apply(sapply(x, function(x) grepl("[0-9]", as.character(x))), 2, any) & sapply(x, function(x) !(is.numeric(x) | inherits(x, 'Date'))) & sapply(x, function(x) length(unique(x))>=k)
  candidate_variables[-select] <- FALSE
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
                             fun="fix_numerics",
                             row.names=NULL)
      changes2 <- do.call(rbind, lapply(changes1$variable, function(y){
        observations <- rownames(x)[which(!(old[, y] %in% x[, y]))]
        tryCatch(data.frame(variable=y,
                            observation=observations,
                            original=old[observations, y],
                            new=x[observations, y],
                            fun="fix_numerics"), error = function(e) NULL)
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
#' @param select Numeric vector with the positions (all by default) to be affected by the function
#' @param track Track changes?
#' @param parallel Should the computations be performed in parallel? Set up strategy first with future::plan()
#' @importFrom future nbrOfWorkers plan
#' @importFrom future.apply future_lapply
#' @export
#' @examples
#' mydata<-data.frame(Dates1=c("25/06/1983", "25-08/2014", "2001/11/01", "2008-10-01"),
#'                    Dates2=c("01/01/85", "04/04/1982", "07/12-2016", "September 24, 2020"),
#'                    Numeric1=rnorm(4))
#' fix_dates(mydata)
fix_dates <- function (x, max.NA=0.8, min.obs=nrow(x)*0.05, use.probs=TRUE, select=1:ncol(x), track=TRUE, parallel=TRUE){
  if(parallel){
    pplan <- attr(future::plan(), "call")
    message("Your parallel configuration is ", if(!is.null(pplan)) pplan else "single core")
    split_factor <- factor(sample(1:future::nbrOfWorkers(), dim(x)[1], replace=TRUE))
    split_x <- split(x, split_factor)
    suppressMessages(
      split_proc <- future.apply::future_lapply(split_x, function(x) fix_dates(x, max.NA=0.8, min.obs=nrow(x)*0.05, use.probs=TRUE, select=1:ncol(x), track=TRUE, parallel=FALSE))
    )
    x <- unsplit(split_proc, f=split_factor)
    changes_par <- do.call(rbind, lapply(split_proc, function(x) attributes(x)$changes))
    attr(x, "changes") <- changes_par[!duplicated(changes_par),]
    attr(x, "messages") <- colSums(do.call(rbind, lapply(split_proc, function(x) attributes(x)$messages)))/c(1, rep(future::nbrOfWorkers(), 2))
  } else{
    changes_old <- attr(x, "changes")
    old <- x
    x<-kill.factors(x)
    x.old<-x
    previous.NA <- sapply(x, function(x) sum(is.na(x)))
    previous.minobs <- sum(sapply(x, function(x) sum(!is.na(x))<min.obs))
    candidate_variables <- apply(sapply(x, function(x) grepl("(-{1}|/{1}).{1,4}(-{1}|/{1})", as.character(x))), 2, any) &
      sapply(x, function(x) class(x)!="Date")
    candidate_variables[-select] <- FALSE
    x[, candidate_variables] <- lapply(x[, candidate_variables, drop = FALSE], function(x) fxd(x, use.probs=use.probs))
    final.NA <- sapply(x, function(x) sum(is.na(x))) - previous.NA
    final.minobs<-sum(sapply(x, function(x) sum(!is.na(x))<min.obs))
    x[,((final.NA-previous.NA) > nrow(x)*max.NA) | sapply(x, function(x) sum(!is.na(x))<min.obs)]<-x.old[,((final.NA-previous.NA) > nrow(x)*max.NA) | sapply(x, function(x) sum(!is.na(x))<min.obs)]
    messages <- data.frame(new_missings = sum(sapply(x, function(x) sum(is.na(x)))-previous.NA),
                           excl_max_NA = sum((final.NA-previous.NA) > nrow(x)*max.NA),
                           excl_minobs = final.minobs-previous.minobs)
    attr(x, "messages") <- messages
    final_variables <- candidate_variables & !(((final.NA-previous.NA) > nrow(x)*max.NA) | sapply(x, function(x) sum(!is.na(x))<min.obs))
    if(track & sum(final_variables)>0){
      changes1 <- data.frame(variable=names(final_variables[final_variables]),
                             observation="all",
                             original=sapply(old[,final_variables, drop=FALSE], class),
                             new="Date",
                             fun="fix_dates",
                             row.names=NULL)
      changes2 <- do.call(rbind, lapply(changes1$variable, function(y){
        observations <- rownames(x)[which(!(as.character(old[, y]) %in% as.character(x[, y])))]
        tryCatch(data.frame(variable=y,
                            observation=observations,
                            original=old[observations, y],
                            new=as.character(x[observations, y]),
                            fun="fix_dates"), error = function(e) NULL)
        }))
      changes <- rbind(changes1, changes2)
      if(!is.null(changes_old)){
        attr(x, "changes") <- rbind(changes_old, changes)
        } else {
          attr(x, "changes") <- changes
        }
    }}
  message(attr(x, "messages")[1], " new missing values generated")
  message(attr(x, "messages")[2], " variables excluded following max.NA criterion")
  message(attr(x, "messages")[3], " variables excluded following min.obs criterion")
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

#' Internal function to fix_dates
#'
#' @description Function to format dates
#' @param d A character vector
#' @param use.probs Solve ambiguities by similarity to the most frequent formats
fxd <- function (d, use.probs = TRUE){
  formats <- c("%d-%m-%Y", "%Y-%m-%d", "%m-%d-%Y",
               "%d-%b-%Y", "%d-%B-%Y", "%y%b%d", "%d%b%y",
               "%d%m%Y", "%Y%m%d", "%m%d%Y", "%d%b%Y",
               "%d%B%Y")
  Sys.setlocale("LC_TIME", "C")
  prueba <- lapply(formats, function(x) as.Date(tolower(gsub("--",
                                                             "-", gsub("[[:punct:]]", "-", gsub("[[:space:]]+", "",
                                                                                                d)))), format = x))
  text_dates <- do.call(c, lapply(d, text_date))
  text_dates2 <- do.call(c, lapply(d, function(x) {
    if (any(as.numeric(unlist(regmatches(x, gregexpr("[0-9]+",
                                                     x)))) >= 32)) {
      text_date(x, format = "%d/%y %b")
    }
    else NA
  }))
  prueba[[13]] <- text_dates
  prueba[[14]] <- text_dates2
  co <- lapply(prueba, function(x){
    tryCatch(x[nchar(gsub("[^\\d]+", "", d, perl=TRUE)) == 8 & as.numeric(format(x, "%Y")) < 100] <- NA, error=function(e) rep(NA, length(x)))
    x
  })
  if (use.probs) {
    co <- co[order(unlist(lapply(co, function(x) sum(is.na(x)))))]
  }
  final_dates <- do.call("c", lapply(1:length(d), function(y){
    na.omit(do.call("c", lapply(co, function(x) x[y])))[1]
  }
  ))
  median_year <- median(as.numeric(format(final_dates, "%Y")), na.rm = TRUE)
  final_dates[as.numeric(format(final_dates, "%Y")) < 100 & !is.na(final_dates)] <- do.call("c", lapply(final_dates[as.numeric(format(final_dates, "%Y")) < 100 & !is.na(final_dates)], function(x){
    years <- as.numeric(format(x, "%Y"))
    posibles <- (years + ((median_year%/%100)+c(-1, 0, 1))*100)
    seq.Date(x, by=paste((posibles - years)[which.min(abs(median_year - posibles))], "years"), length.out=2)[2]
  }))
  Sys.setlocale("LC_TIME", "")
  final_dates
}

#' Fix levels
#'
#' @description Fixes levels of a factor
#' @param data data.frame with the factor to fix
#' @param factor_name Name of the factor to fix (as character)
#' @param method Method from stringdist package to estimate distances
#' @param levels Optional vector with the levels names. If "auto", levels are assigned based on frequency
#' @param plot Optional: Plot cluster dendrogram?
#' @param k Number of levels for clustering
#' @param track Keep track of changes?
#' @param ... Further parameters passed to stringdist::stringdistmatrix function
#' @importFrom stats hclust rect.hclust cutree
#' @export
#' @examples
#' mydata <- data.frame(factor1=factor(c("Control", "Treatment", "Tretament", "Tratment", "treatment",
#' "teatment", "contrl", "cntrol", "CONTol", "not available", "na")))
#' fix_levels(mydata, "factor1", k=4, plot=TRUE)   #Chose k to select matching levels
#' fix_levels(mydata, "factor1", levels=c("Control", "Treatment"), k=4)
fix_levels <- function(data, factor_name, method="dl", levels=NULL, plot=FALSE, k=ifelse(!is.null(levels), length(levels), 2), track=TRUE, ...){
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
    if(length(levels) == 1 && levels == "auto") levels <- sapply(split(names(groups), groups), function(x) names(sort(table(x), decreasing=TRUE))[1])
    assig_lvls <- apply(sapply(split(as.data.frame(stringdist::stringdistmatrix(tolower(x_na), tolower(levels), method=method, useNames = TRUE)), groups), colMeans), 1, which.min)
    output <- factor(groups, levels=assig_lvls, labels=names(assig_lvls))
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
                          fun="fix_levels",
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

#' fix_NA
#'
#' @description Fixes miscoded missing values
#' @param x A data.frame
#' @param na.strings Strings to be considered NA
#' @param track Track changes?
#' @export
#' @examples
#' mydata <- data.frame(prueba = c("", NA, "A", 4, " ", "?", "-", "+"),
#' casa = c("", 1, 2, 3, 4, " ", 6, 7))
#' fix_NA(mydata)
fix_NA <- function(x, na.strings=c("^$", "^ $", "^\\?$", "^-$", "^\\.$", "^NaN$", "^NULL$", "^N/A$"), track=TRUE){
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
                 fun="fix_NA",
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

#' fix_concat
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
#' fix_concat(mydata, "concat")
fix_concat <- function(x, varname, sep=", |; | ", track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  new_vars <- sapply(unique(unlist(strsplit(as.character(x[,varname]), sep))), function(y) as.numeric(grepl(y, x[,varname])))
  colnames(new_vars) <- paste(varname, colnames(new_vars), sep="_")
  colnames(new_vars) <- iconv(gsub("x_","",gsub("_$", "",tolower(gsub("[_]+", "_",gsub("[.]+", "_",make.names(
    gsub("^[ ]+", "",gsub("%", "percent",gsub("\"", "",gsub("'", "",gsub("\u00BA", "", colnames(new_vars)))))))))))), to = "ASCII//TRANSLIT")
  x <- data.frame(x, new_vars)
  if(!identical(old, x)){
    if(track){
      changes <- data.frame(variable=colnames(new_vars),
                            observation="all",
                            original=NA,
                            new="logical",
                            fun="fix_concat", row.names=NULL)
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
#' @param remove_rows Remove empty rows?
#' @param remove_cols Remove empty columns?
#' @param track Track changes?
#' @export
#' @examples
#' mydata <- data.frame(a = c(NA, NA, NA, NA, NA), b = c(1, NA, 3, 4, 5),
#' c=c(NA, NA, NA, NA, NA), d=c(4, NA, 5, 6, 3))
#' remove_empty(mydata)
remove_empty <- function(x, remove_rows=TRUE, remove_cols=TRUE, track=TRUE){
  changes_old <- attr(x, "changes")
  old <- x
  if(remove_rows){
    empty_rows <- apply(x, 1, function(x) all(is.na(x)))
    x <- x[!empty_rows, ]
  }
  if(remove_cols){
    empty_cols <- apply(x, 2, function(x) all(is.na(x)))
    x <- x[, !empty_cols]
  }
  if(!identical(old, x)){
    if(track){
      if(exists("empty_cols") && sum(empty_cols)>0){
        changes_col <- data.frame(variable=names(old)[!names(old) %in% names(x)],
                                  observation="all",
                                  original=NA,
                                  new="removed",
                                  fun="remove_empty", row.names=NULL)
      } else changes_col <- NULL
      if(exists("empty_rows") && sum(empty_rows)>0){
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

#' fix_all
#'
#' @description Tries to automatically fix all problems in the data.frame
#' @param x A data.frame
#' @param select Numeric vector with the positions (all by default) to be affected by the function
#' @param track Track changes?
#' @export
fix_all <- function(x, select=1:ncol(x), track=TRUE){
  x <- fix_numerics(
    fix_factors(
      fix_dates(
        remove_empty(
          fix_NA(
            nice_names(x,
                       select=select,
                       track=track),
            track=track),
          track=track),
        select=select,
        track=track),
      select=select,
      track=track),
    select=select,
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
#' mydata <- fix_dates(mydata)
#' mydata
#' track_changes(mydata)
track_changes <- function(x, subset){
  changes <- attr(x, "changes")
  if(is.null(changes)){
    changes
  } else{
    attr(changes, "data") <- as.character(match.call()$x)
    if(missing(subset)) f <- rep(TRUE, nrow(changes)) else f <- eval(substitute(subset), changes, baseenv())
    print(changes[f,], row.names=FALSE)
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
#' mydata <- fix_dates(mydata)
#' mydata
#' tracking <- track_changes(mydata)
#' mydata_r <- restore_changes(tracking)
#' mydata_r
restore_changes <- function(tracking){
  data <- get(attr(tracking, "data"))
  old_changes <- attr(data, "changes")
  track_id <- paste(tracking$variable[tracking$fun != "manual_fix"], tracking$observation[tracking$fun!= "manual_fix"], sep=", ")
  old_track_id <- paste(old_changes$variable, old_changes$observation, sep=", ")
  conflict_tracks <- old_changes[old_track_id %in% track_id[track_id %in% old_track_id[old_changes$fun=="manual_fix"]],]
  if(nrow(conflict_tracks) > 1) {
    print(conflict_tracks)
    stop("Cannot restore changes applied to observations before a manual fix. \n Please restore the manual fix first.")
  }
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
  if(any(tracking$fun == "fix_concat")) data <- data[,!names(data) %in% tracking$variable[tracking$fun == "fix_concat"]]
  trackingf <- tracking[!tracking$fun %in% c("nice_names", "remove_empty", "fix_concat"),]
  variables <- unique(trackingf$variable)
  # data[, variables] <- lapply(variables, function(y){
  #   changes.y <- trackingf[trackingf$variable == y,]
  #   old.class <- changes.y$original[changes.y$observation == "all"][1]
  #   data[, y] <- as.character(data[, y])
  #   if(!is.na(old.class)) class(data[, y]) <- old.class
  #   data[changes.y$observation[changes.y$observation != "all"], y] <- changes.y$original[changes.y$observation != "all"]
  #   data[, y]
  # })
  data[, variables] <- lapply(variables, function(y){
    changes.y <- trackingf[trackingf$variable == y,]
    old.class <- changes.y$original[changes.y$observation == "all"][1]
    data_n <- as.character(data[, y])
    if(!is.na(old.class)) class(data_n) <- old.class
    #data_n[rownames(data) %in% changes.y$observation[changes.y$observation != "all"]] <- changes.y$original[changes.y$observation != "all"]
    data_n[rownames(data) %in% changes.y$observation[changes.y$observation != "all"]] <- changes.y$original[changes.y$observation != "all"][match(
      rownames(data)[rownames(data) %in% changes.y$observation[changes.y$observation != "all"]], changes.y$observation[changes.y$observation != "all"])]
    data_n
  })
  if(nrow(varnames)>0){
    names(data)[names(data) %in% varnames$variable] <- na.omit(varnames$original[match(names(data), varnames$new)])
    old_changes$variable[old_changes$variable != "all"  & !is.na(old_changes$variable)] <- old_changes$original[old_changes$fun == "nice_names"][match(old_changes$variable[old_changes$variable!="all"  & !is.na(old_changes$variable)], old_changes$new[old_changes$fun == "nice_names"])]
    old_changes <- old_changes[!(old_changes$variable %in% varnames$original & old_changes$fun == "remove_empty"),]
  }
  #changes <- old_changes[! apply(old_changes[,-1], 1, function(x) paste(x, collapse="")) %in% apply(tracking[,-1], 1, function(x) paste(x, collapse="")), ]
  changes <- old_changes[! apply(old_changes, 1, function(x) paste(x, collapse="")) %in% apply(tracking, 1, function(x) paste(x, collapse="")), ]
  changes <- changes[!changes$observation %in% create_rows & !changes$variable %in% create_vars,]
  attr(data, "changes") <- changes
  if(all(may.numeric(rownames(data)))) data[order(as.numeric(rownames(data))),] else data
}

#' Tracked manual fixes to data
#'
#' @description Tracks manual fixes performed on a variable in a data.frame
#' @param data A data.frame
#' @param variable A character string with the name of the variable to be fixed
#' @param subset A logical expression for selecting the cases to be fixed
#' @param newvalues New value or values that will take the cases selected by \code{subset} parameter.
#' @export
#' @examples
#' iris2 <- manual_fix(iris, "Petal.Length", Petal.Length < 1.2, 0)
#' track_changes(iris2)
manual_fix <- function(data, variable, subset, newvalues=NULL){
  old_data <- data
  changes_old <- attr(data, "changes")
  f <- eval(substitute(subset), data, .GlobalEnv)
  if(!is.null(newvalues)){
    data[f, variable] <- newvalues
    observations <- rownames(data)[which(f)]
    changes <- data.frame(variable=variable,
                          observation=observations,
                          original=old_data[observations, variable],
                          new=data[observations, variable],
                          fun="manual_fix",
                          row.names=NULL)
    if(!is.null(changes_old)){
      attr(data, "changes") <- rbind(changes_old, changes)
    } else {
      attr(data, "changes") <- changes
    }
    data
  } else data[f, variable]
}


#' Find and replace
#'
#' @description Searches a data.frame for a specific character string and replaces it with another one
#' @param x A data.frame
#' @param string A character string to search in the data.frame
#' @param replacement A character string to replace the old string (can be NA)
#' @param complete If TRUE, search for complete strings only. If FALSE, search also for partial strings.
#' @param select Numeric vector with the positions (all by default) to be affected by the function
#' @param track Track changes?
#' @export
#' @examples
#' iris2 <- f_replace(iris, "setosa", "ensata")
#' track_changes(iris2)
f_replace <- function(x, string, replacement, complete=TRUE, select=1:ncol(x), track=TRUE){
  old_data <- x
  changes_old <- attr(x, "changes")
  if(complete) string <- paste("^", string, "$", sep="")
  candidate_variables <- sapply(x, function(x) any(grep(string, x)))
  candidate_variables[-select] <- FALSE
  x[,candidate_variables] <- as.data.frame(lapply(x[,candidate_variables, drop=FALSE], function(x) {
      kk <- class(x)
      x <- gsub(string, replacement, x)
      eval(parse(text=paste("as.", kk, "(x)", sep="")))
  }))
  if(track){
    variables <- names(x)[candidate_variables]
    changes <- do.call(rbind, lapply(variables, function(y){
      observations <- rownames(old_data)[grepl(string, old_data[, y])]
      data.frame(variable=y,
                 observation=observations,
                 original=old_data[observations, y],
                 new=x[observations, y],
                 fun="f_replace",
                 row.names=NULL)
    }))
    if(!is.null(changes_old)){
      attr(x, "changes") <- rbind(changes_old, changes)
    } else {
      attr(x, "changes") <- changes
    }
  }
  x
}
