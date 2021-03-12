#' Report from linear model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a linear model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A linear model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats confint getCall
#' @export
report.lm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.lm' will be removed in the next version of the package. See help('clickR-deprecated')")
  sx <- summary(x)
  ci <- confint(x)
  obj <- list(coefficients=setNames(sx$coefficients[,1], rownames(sx$coefficients)), se=sx$coefficients[,2], lwr.int=ci[,1],
              upper.int=ci[,2], pvalues=sx$coefficients[,4], r.squared=sx$r.squared, adj.r.squared=sx$adj.r.squared)
  output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),
                      round(obj$lwr.int,digits),round(obj$upper.int, digits), round(obj$pvalues,digitspvals)),
                c(round(obj$r.squared,digits+1),rep("",4)),
                c(round(obj$adj.r.squared,digits+1),rep("",4)))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[c(dim(sx$coefficients)[1]+1,dim(sx$coefficients)[1]+2)]<-c('R Squared','Adj.R Squared')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from generalized linear model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a generalized linear model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A generalized linear model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.glm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.glm' will be removed in the next version of the package. See help('clickR-deprecated')")
  compute.exp<-x$family$link %in% c("logit", "log")
  sx<-summary(x)
  ci<-confint(x)
  obj <- list(coefficients=setNames(sx$coefficients[,1], rownames(sx$coefficients)), se=sx$coefficients[,2], lwr.int=ci[,1],
              upper.int=ci[,2], pvalues=sx$coefficients[,4], aic=sx$aic)
  if(compute.exp){
    obj$exp.coef <- exp(sx$coefficients[,1])
    obj$exp.lwr.int <- exp(ci[,1])
    obj$exp.upper.int <- exp(ci[,2])
  }
  output<-rbind(cbind(round(obj$coefficients,digits), round(obj$se, digits),
                      if(compute.exp) {
                        cbind(round(obj$exp.coef,digits), round(obj$exp.lwr.int, digits),
                      round(obj$exp.upper.int, digits))
                      } else{
                          cbind(round(obj$lwr.int, digits), round(obj$upper.int, digits))
                        }
                      , round(obj$pvalues,digitspvals)),
                c(round(obj$aic,digits),rep("",ifelse(compute.exp, 5, 4))))
  colnames(output)<-c('Estimate','Std. Error',if(compute.exp) {'exp(Estimate)'},'Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$coefficients)[1]+1]<-c('AIC')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from cox regression model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a cox model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A cox model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats AIC getCall
#' @export
report.coxph<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.coxph' will be removed in the next version of the package. See help('clickR-deprecated')")
  sx<-summary(x)
  obj <- list(coefficients=setNames(sx$coefficients[,1], rownames(sx$coefficients)), se=sx$coefficients[,3], hr=sx$conf.int[,1],
              lwr.int=sx$conf.int[,3], upper.int=sx$conf.int[,4], pvalues=sx$coefficients[,5], aic=AIC(x))
  output<-rbind(cbind(round(obj$coefficients,digits), round(obj$se, digits), round(obj$hr, digits),
                      round(obj$lwr.int,digits), round(obj$upper.int, digits),
                      round(obj$pvalues,digitspvals)),c(round(obj$aic,digits),rep('',5)))
  colnames(output)<-c('Estimate','Std. Error','HR','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(output)[1]]<-c('AIC')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from linear mixed model with pvalues
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a linear mixed model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A linear mixed model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @importFrom methods loadMethod
#' @export
report.merModLmerTest<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.merModLmerTest' will be removed in the next version of the package. See help('clickR-deprecated')")
  #loadNamespace("lmerTest")
  loadMethod("summary", "summary.lmerModLmerTest", envir="lmerTest")
  sx <- summary(x)
  #unloadNamespace("lmerTest")
  cor <- as.data.frame(lme4::VarCorr(x))
  ci <- confint(x)
  #cor[dim(cor)[1],2]<-'Residual'
  obj<- list(coefficients=setNames(sx$coefficients[,1], rownames(sx$coefficients)), se=sx$coefficients[,2], lwr.int=ci[,1][-c(1:dim(as.data.frame(lme4::VarCorr(x)))[1])],
             upper.int=ci[,2][-c(1:dim(as.data.frame(lme4::VarCorr(x)))[1])],
             pvalues=tryCatch(sx$coefficients[,5], error=function(x) NA), aic=AIC(x),
             random=cor[c(is.na(cor$var2)),c(5)])
  output<-rbind(rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),
                            round(obj$lwr.int,digits), round(obj$upper.int, digits),
                            round(obj$pvalues, digits)),
                      c(round(obj$aic,digits-1),rep("",4))),
                matrix(c(round(obj$random,digits),rep("",4*dim(cor[c(is.na(cor$var2)),])[1])),ncol=5,byrow=F))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$coefficients)[1]+1]<-c('AIC')
  rownames(output)[rownames(output)==""]<-paste(c(rep('Sd ',length(cor[is.na(cor$var2), c(2)])-1), ""),
                                                na.omit(cor[is.na(cor$var2), c(1)]),
                                                c(na.omit(cor[is.na(cor$var2), c(2)]), ""),sep='')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from linear mixed model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a linear mixed model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A linear mixed model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.lmerMod<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.lmerMod' will be removed in the next version of the package. See help('clickR-deprecated')")
  x<-lmerTest::lmer(x@call,data=x@frame)
  report.merModLmerTest(x, file, type, digits, digitspvals, info=info, ...)
}

#' Report from generalized linear mixed model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a generalized linear mixed model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A generalized linear mixed model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.glmerMod<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.glmerMod' will be removed in the next version of the package. See help('clickR-deprecated')")
  compute.exp<-x@resp$family$link %in% c("logit", "log")
  sx<-summary(x)
  cor<-as.data.frame(lme4::VarCorr(x))
  ci <- confint(x)
  obj<- list(coefficients=setNames(sx$coefficients[,1], rownames(sx$coefficients)), se=sx$coefficients[,2], lwr.int=ci[,1][-c(1:dim(as.data.frame(lme4::VarCorr(x)))[1])],
             upper.int=ci[,2][-c(1:dim(as.data.frame(lme4::VarCorr(x)))[1])],
             pvalues=sx$coefficients[,4], aic=AIC(x),
             random=cor[c(is.na(cor$var2)),c(5)])
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
    obj$exp.lwr.int <- exp(obj$lwr.int)
    obj$exp.upper.int <- exp(obj$upper.int)
  }
  output<-rbind(rbind(cbind(round(obj$coefficients,digits),
                            round(obj$se, digits),
                            if(compute.exp) {
                              cbind(round(obj$exp.coef,digits), round(obj$exp.lwr.int, digits),
                                    round(obj$exp.upper.int, digits))
                            } else{
                              cbind(round(obj$lwr.int, digits), round(obj$upper.int, digits))
                            }
                            , round(obj$pvalues,digitspvals)),
                      c(round(obj$aic, digits), rep("", ifelse(compute.exp, 5, 4)))),
                matrix(c(round(obj$random,digits),rep("",ifelse(compute.exp, 5, 4)*dim(cor[c(is.na(cor$var2)),])[1])),ncol=ifelse(compute.exp, 6, 5),byrow=F))
  colnames(output)<-c('Estimate','Std. Error', if(compute.exp) 'exp(Estimate)','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$coefficients)[1]+1]<-c('AIC')
  rownames(output)[rownames(output)==""]<-paste(rep('Sd ',length(cor[is.na(cor$var2), c(2)])),
                                                na.omit(cor[is.na(cor$var2), c(1)]),
                                                na.omit(cor[is.na(cor$var2), c(2)]),sep='')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from quantile mixed model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a quantile mixed model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A quantile model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.lqmm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.lqmm' will be removed in the next version of the package. See help('clickR-deprecated')")
  sx<-summary(x, ...)
  obj<-list(coefficients=setNames(sx$tTable[,1], rownames(sx$tTable)), se=sx$tTable[,2], lwr.int=sx$tTable[,3], upper.int=sx$tTable[,4],
            pvalues=sx$tTable[,5], aic=sx$aic, random=round(VarCorr(x),2))
  output<-rbind(rbind(cbind(round(obj$coefficients,digits), round(obj$se,digits),
                            round(obj$lwr.int, digits), round(obj$upper.int, digits), round(obj$pvalues, digitspvals)),
                      c(round(obj$aic, digits),rep("",4))),
                matrix(c(round(obj$random,digits),rep("",4*length(obj$random))),ncol=5,byrow=F))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$tTable)[1]+1]<-c('AIC')
  rownames(output)[(dim(sx$tTable)[1]+2):(((dim(sx$tTable)[1]+2)+length(obj$random))-1)]<-paste('Ran.Eff',names(VarCorr(x)),sep=' ')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from ordinal model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from an ordinal model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x An ordinal model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.clm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.clm' will be removed in the next version of the package. See help('clickR-deprecated')")
  compute.exp<-x$link %in% c("logit", "log")
  sx<-summary(x)
  ci<-confint(x)
  obj<-list(coefficients=sx$coefficients[,1][-c(1:length(x$alpha))], se=sx$coefficients[,2][-c(1:length(x$alpha))],
            lwr.int=ci[,1], upper.int=ci[,2], pvalues=sx$coefficients[,4][-c(1:length(x$alpha))], aic=AIC(x))
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
    obj$exp.lwr.int <- exp(obj$lwr.int)
    obj$exp.upper.int <- exp(obj$upper.int)
  }
  obj$thresholds <- sx$coefficients[1:length(x$alpha),]
  output<-rbind(cbind(round(obj$coefficients,digits), round(obj$se,digits),
                      if(compute.exp) {
                        cbind(round(obj$exp.coef, digits), round(obj$exp.lwr.int,digits),
                              round(obj$exp.upper.int, digits))
                      } else{
                        cbind(round(obj$lwr.int, digits), round(obj$upper.int, digits))
                      }
                      , round(obj$pvalues,digitspvals)), c(round(obj$aic,digits),rep("", ifelse(compute.exp, 5, 4))))
  colnames(output)<-c('Estimate','Std. Error',if(compute.exp) 'exp(Estimate)','Lower 95%','Upper 95%','P-value')
  rownames(output)[length(rownames(output))]<-c('AIC')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from ordinal mixed model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from an ordinal mixed model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x An ordinal model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.clmm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.clmm' will be removed in the next version of the package. See help('clickR-deprecated')")
  compute.exp<-x$link %in% c("logit", "log")
  sx<-summary(x)
  ci<-confint(x)
  obj <- list(coefficients=sx$coefficients[,1][-c(1:length(x$alpha))], se=sx$coefficients[,2][-c(1:length(x$alpha))],
              lwr.int=ci[,1][-c(1:length(x$alpha))], upper.int=ci[,2][-c(1:length(x$alpha))], pvalues=sx$coefficients[,4][-c(1:length(x$alpha))], aic=AIC(x),
              random=VarCorr(x))
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
    obj$exp.lwr.int <- exp(obj$lwr.int)
    obj$exp.upper.int <- exp(obj$upper.int)
  }
  obj$thresholds <- sx$coefficients[1:length(x$alpha),]
  output<-rbind(rbind(cbind(round(obj$coefficients,digits), round(obj$se, digits),
                            if(compute.exp) {
                              cbind(round(obj$exp.coef,digits), round(obj$exp.lwr.int, digits),
                                    round(obj$exp.upper.int, digits))
                            } else{
                              cbind(round(obj$lwr.int, digits), round(obj$upper.int, digits))
                            }
                             , round(obj$pvalues,digitspvals)), c(round(obj$aic,digits),rep("",ifelse(compute.exp, 5, 4)))),
                matrix(c(round(rapply(obj$random, function(x) sqrt(diag(x))),digits),
                         rep("",ifelse(compute.exp, 5, 4)*length(rapply(obj$random, function(x) sqrt(diag(x)))))),ncol=ifelse(compute.exp, 6, 5),byrow=F))
  colnames(output)<-c('Estimate','Std. Error', if(compute.exp) 'exp(Estimate)','Lower 95%','Upper 95%','P-value')
  rownames(output)[length(x$beta)+1]<-c('AIC')
  rownames(output)[rownames(output)==""]<-names(rapply(VarCorr(x), function(x) sqrt(diag(x))))
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from quantile regression model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a quantile regression model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A quantreg model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.rq<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.rq' will be removed in the next version of the package. See help('clickR-deprecated')")
  sx<-summary(x, se="rank")
  sx2<-summary(x, covariance=TRUE)
  obj<-list(coefficients=setNames(sx$coefficients[,1], rownames(sx$coefficients)), se=sx2$coefficients[,2], lwr.int=sx$coefficients[,2],
            upper.int=sx$coefficients[,3], pvalues=sx2$coefficients[,4], aic=AIC(x))
  output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),
                      round(obj$lwr.int,digits), round(obj$upper.int, digits),
                      round(obj$pvalues,digitspvals)),
                c(round(obj$aic, digits),rep("",4)))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$coefficients)[1]+1]<-'AIC'
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from beta regression model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a beta regression model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A betareg model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.betareg<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.betareg' will be removed in the next version of the package. See help('clickR-deprecated')")
  compute.exp<-x$link$mean$name %in% c("logit", "log")
  sx<-summary(x)
  ci<-confint(x)
  obj<-list(coefficients=setNames(sx$coefficients$mean[,1], rownames(sx$coefficients$mean)), se=sx$coefficients$mean[,2], lwr.int=ci[,1][-dim(ci)[1]],
       upper.int=ci[,2][-dim(ci)[1]], pvalues=sx$coefficients$mean[,4], aic=AIC(x), pseudo.r=sx$pseudo.r.squared,
       phi=sx$coefficients$precision)
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
    obj$exp.lwr.int <- exp(obj$lwr.int)
    obj$exp.upper.int <- exp(obj$upper.int)
  }
  output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),
                      if(compute.exp) {
                        cbind(round(obj$exp.coef,digits), round(obj$exp.lwr.int, digits),
                              round(obj$exp.upper.int, digits))
                      } else{
                        cbind(round(obj$lwr.int, digits), round(obj$upper.int, digits))
                      }
                      , round(obj$pvalues, digitspvals)), c(round(obj$phi[1], digits+1),rep("", ifelse(compute.exp, 5, 4))),
                c(round(obj$pseudo.r, digits), rep("", ifelse(compute.exp, 5, 4))))
  colnames(output)<-c('Estimate','Std. Error',if(compute.exp) 'exp(Estimate)', 'Lower 95%','Upper 95%','P-value')
  rownames(output)[c(dim(sx$coefficients$mean)[1]+1, dim(sx$coefficients$mean)[1]+2)]<-c("phi", "Pseudo R-squared")
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report models from brms package
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from model fitted by brms. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A brms model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.brmsfit<-function(x, file=NULL, type="word", digits=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.brmsfit' will be removed in the next version of the package. See help('clickR-deprecated')")
  compute.exp<-x$family$link %in% c("logit", "log")
  sx<-summary(x)
  WC<-eval(parse(text="brms::WAIC(x)"))
  random<-tryCatch(do.call(rbind, sx$random), error=function(e) NA)
  if(!any(is.na(random))) rownames(random)<-paste(rownames(random),rep(names(sx$random), sapply(sx$random, nrow)), sep=" ")
  obj<-list(coefficients=setNames(sx$fixed[,1], rownames(sx$fixed)), se=sx$fixed[,2], lwr.int=sx$fixed[,3],
            upper.int=sx$fixed[,4], random=random, WAIC=setNames(c(WC$estimates[3,1], WC$estimates[3,2]), c("WAIC", "WAIC SE")), Eff.Sample_min=min(c(sx$fixed[,6], sx$fixed[,7])), Rhat_max=round(max(sx$fixed[,5]),2))
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
    obj$exp.lwr.int <- exp(obj$lwr.int)
    obj$exp.upper.int <- exp(obj$upper.int)
  }
  output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),
                      if(compute.exp){
                        cbind(round(obj$exp.coef, digits), round(obj$exp.lwr.int, digits),
                              round(obj$exp.upper.int, digits))
                      } else{
                        cbind(round(obj$lwr.int,digits), round(obj$upper.int, digits))
                      }), if(!any(is.na(random))) {cbind(round(random[,1:2, drop=FALSE], digits), if(compute.exp) "-", round(random[,3:4, drop=FALSE], digits))},
                c(round(WC$estimates[3,1], digits), round(WC$estimates[3,2], digits), rep("", ifelse(compute.exp, 3, 2))))
  rownames(output)[dim(output)[1]]<-"WAIC"
  colnames(output)<-c('Estimate','Std. Error',if(compute.exp) 'exp(Estimate)', 'Lower 95%','Upper 95%')
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    suppressWarnings(make_table(output, file, type, info=info, ...))
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  if(obj$Rhat_max > 1.1) warning("Please diagnose your model, Rhat values greater than 1.1")
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report models from glmnet package
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from models fitted by glmnet. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A glmnet model object
#' @param s Value of lambda for estimating the coefficients
#' @param gamma Value of gamma for estimating the coefficients (only used in relaxed fits)
#' @param drop.zero Should zero coefficients be dropped?
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats coef getCall
#' @export
report.glmnet<-function(x, s, gamma=1, drop.zero=TRUE, file=NULL, type="word", digits=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.glmnet' will be removed in the next version of the package. See help('clickR-deprecated')")
  compute.exp<- any(grepl("binomial|cox", x$call))
  if("relaxed" %in% class(x)){
    coefs <- coef(x, s=s, gamma=gamma)
  } else {
    coefs <- coef(x, s=s)
  }
  if("multnet" %in% class(x)){
    temp_coef <- setNames(data.frame(as.matrix(do.call(cbind, coefs))), names(coefs))
    obj <- list(coefficients=temp_coef[apply(temp_coef, 1, function(x) any(x != 0)),], lwr.int=NA, upper.int=NA)
  } else {
    obj <- list(coefficients=as.numeric(coefs)[if(drop.zero) {as.numeric(coefs)!=0}], lwr.int=NA, upper.int=NA)
    names(obj$coefficients)<-rownames(coefs)[if(drop.zero) {as.numeric(coefs)!=0}]
  }
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
  }
  obj$lambda <- s
  if("multnet" %in% class(x)){
    output <- rbind(round(obj$coefficients, digits), c(s, rep("", ifelse(compute.exp, max(ncol(obj$coefficients), 1), max(ncol(obj$coefficients)-1, 0)))))
    rownames(output)[nrow(output)] <- "lambda"
  } else {
    output<-rbind(cbind(round(obj$coefficients,digits),
                        if(compute.exp){
                          round(obj$exp.coef, digits)
                          }), cbind(s, rep("", ifelse(compute.exp, 1, 0))))
    colnames(output)<-c('Estimate', if(compute.exp) 'exp(Estimate)')
    rownames(output)<-c(names(obj$coefficients), "lambda")
  }
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from robust linear model (rlm)
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a robust linear model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A rlm object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.rlm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.rlm' will be removed in the next version of the package. See help('clickR-deprecated')")
  sx <- summary(x, method = "XtWX")
  ci <- rob.ci(x, ...)
  obj <- list(coefficients=setNames(sx$coefficients[,1], rownames(sx$coefficients)), se=sx$coefficients[,2], lwr.int=ci[,1],
              upper.int=ci[,2], pvalues=rob.pvals(x), AIC=AIC(x))
  output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),
                      round(obj$lwr.int,digits),round(obj$upper.int, digits), round(obj$pvalues,digitspvals)),
                c(round(obj$AIC,digits+1),rep("",4)))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$coefficients)[1]+1]<-'AIC'
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from generalized linear mixed model from ADMB
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table from a glmmadmb model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A generalized linear mixed model object (glmmabmb)
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param print Should the report table be printed on screen?
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.glmmadmb<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, print=TRUE, ...){
  .Deprecated(package="clickR", msg = "'report.glmmadmb' will be removed in the next version of the package. See help('clickR-deprecated')")
  compute.exp<-x$link %in% c("logit", "log")
  sx<-summary(x)
  cor<-sqrt(cbind(unlist(lapply(x$S, function(x) diag(x)))))
  ci <- confint(x)
  obj<- list(coefficients=setNames(sx$coefficients[,1], rownames(sx$coefficients)), se=sx$coefficients[,2], lwr.int=ci[,1],
             upper.int=ci[,2],
             pvalues=sx$coefficients[,4], aic=AIC(x),
             random=cor)
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
    obj$exp.lwr.int <- exp(obj$lwr.int)
    obj$exp.upper.int <- exp(obj$upper.int)
  }
  output<-rbind(rbind(cbind(round(obj$coefficients,digits),
                            round(obj$se, digits),
                            if(compute.exp) {
                              cbind(round(obj$exp.coef,digits), round(obj$exp.lwr.int, digits),
                                    round(obj$exp.upper.int, digits))
                            } else{
                              cbind(round(obj$lwr.int, digits), round(obj$upper.int, digits))
                            }
                            , round(obj$pvalues,digitspvals)),
                      c(round(obj$aic, digits), rep("", ifelse(compute.exp, 5, 4)))),
                matrix(c(round(obj$random,digits),rep("",ifelse(compute.exp, 5, 4)*dim(cor)[1])),ncol=ifelse(compute.exp, 6, 5),byrow=F))
  colnames(output)<-c('Estimate','Std. Error', if(compute.exp) 'exp(Estimate)','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$coefficients)[1]+1]<-c('AIC')
  rownames(output)[rownames(output)==""]<-paste(rep('Sd ',dim(cor)[1]),
                                                rownames(cor),sep='')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    info <- if(info) deparse1(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  if(print) print(obj$output, row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Function to compute p-values for robust linear regression models
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Estimates p-values for rlm models. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A rlm object
#' @return A vector of p-values
#' @importFrom stats pf
#' @export
rob.pvals <- function(x){
  .Deprecated(package="clickR", msg = "'rob.pvals' will be removed in the next version of the package. See help('clickR-deprecated')")
  coefs <- x$coef
  sx<-summary(x, method = "XtWX")
  covs<-diag(sx$cov.unscaled)*sx$stddev^2
  statistics<-sapply(1:length(coefs), function(x) sum(coefs[x]*solve(covs[x], coefs[x])))
  pf(statistics, 1, sx$df[2], lower.tail = FALSE)
}

#' Function to compute bootstrap confidence intervals for robust linear regression models
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Estimates confidence intervals for rlm models. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A rlm object
#' @param level Confidence level for the interval
#' @param maxit Maximum number of iterations per fit
#' @param R Number of boostrap samples
#' @return A matrix with bootstrap confidence intervals for each variable in the model
#' @importFrom stats formula
#' @export
rob.ci <- function(x, level=0.95, maxit=200, R=2000){
  .Deprecated(package="clickR", msg = "'rob.ci' will be removed in the next version of the package. See help('clickR-deprecated')")
  coefb <- function(object, data, indices){      #Function to extract R2
    d <- data[indices,]
    object$call$data <- as.name("d")
    object$call$maxit <- maxit
    fit <- eval(object$call)
    return(coef(fit))
  }
  results <- boot::boot(data=x$model, statistic=coefb, R=R, object=x)
  t(sapply(lapply(1:length(x$coefficients), function(x) boot::boot.ci(results, conf=level, type="bca", index=x)), function(x) x$bca[4:5]))
}

#' Export a table to word
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Exports a table to Word. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A data.frame object
#' @param file Name of the file
#' @param info Footer for the table
#' @param use.rownames Should row names be added to the output?
#' @return Creates a word file with the table
#' @export
make_word_table <- function(x, file, info=NULL, use.rownames=TRUE){
  .Deprecated(package="clickR", msg = "'make_word_table' will be removed in the next version of the package. See help('clickR-deprecated')")
  mydoc <- officer::read_docx()
  dataf <- data.frame(x)
  if(use.rownames) dataf <- data.frame(Variables=rownames(dataf), dataf)
  MyFlexTable <- flextable::flextable(dataf)
  MyFlexTable <- flextable::bold(flextable::fontsize(flextable::font(MyFlexTable,fontname='Calibri',part='all'),
                                                     size = 12, part = "all"), bold = TRUE, part = "header")
  MyFlexTable <- set_noms(MyFlexTable,  setNames(as.list(c(if(use.rownames) {""},
                                                           names(data.frame(x,check.names = F)))),
                                                 MyFlexTable$header$dataset[1,]))
  MyFlexTable <- flextable::autofit(MyFlexTable)
  MyFlexTable <- flextable::hline_bottom(flextable::hline_top(flextable::border_remove(MyFlexTable),part='all',
                border=officer::fp_border(color="black", width = 2)),
                border=officer::fp_border(color="black", width = 2),part='all')
  mydocFlex <- flextable::body_add_flextable(mydoc, MyFlexTable, align="center")
  if(!is.null(info)) mydocFlex <- officer::body_add_par(mydocFlex, info)
  print(mydocFlex,target= paste(file, ".docx", sep = ""))
}

#' Export a table to latex
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Exports a table to latex. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A data.frame object
#' @param file Name of the file
#' @return Creates a .txt file with latex code for the table
#' @export
make_latex_table <- function(x, file){
  .Deprecated(package="clickR", msg = "'make_latex_table' will be removed in the next version of the package. See help('clickR-deprecated')")
  print(xtable::xtable(x), file=paste(file, ".txt", sep=""))
}

#' Export a table to excel
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Exports a table to Excel. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A data.frame object
#' @param file Name of the file
#' @param info Footer for the table
#' @importFrom utils write.csv2
#' @return Creates a .csv file with the table
#' @export
make_csv_table <- function(x, file, info){
  .Deprecated(package="clickR", msg = "'make_csv_table' will be removed in the next version of the package. See help('clickR-deprecated')")
  write.csv2(data.frame(rbind(x, c(info, rep("", ncol(x)-1))), check.names=FALSE,
                        stringsAsFactors=FALSE), paste(file, ".csv", sep=""),
             row.names=TRUE)
}


#' Make a table from report
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Auxiliary function to create tables. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A data.frame object
#' @param file Name of the file
#' @param type Type of file
#' @param info Footer for the table
#' @param ... Additional parameters passed to make_word_table
#' @return Creates a file with the table
#' @export
make_table<-function(x, file, type, info=NULL, ...){
  .Deprecated(package="clickR", msg = "'make_table' will be removed in the next version of the package. See help('clickR-deprecated')")
  if(type=="csv") {make_csv_table(x, file, info)}
  if(type=="latex") {make_latex_table(x, file)}
  if(is.null(type) | type=="word") {make_word_table(x, file, info, ...)}
  message(paste0("Exported table as ", file))
}

#' Generic VarCorr function
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Extract Variance-Covariance Matrix. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A model object
#' @param sigma Optional value used as a multiplier for the standard deviations
#' @param ... Further arguments passed to VarrCorr methods
#' @return A Variance-Covariance Matrix
#' @export
VarCorr<-function (x, sigma = 1, ...){
  .Deprecated(package="clickR", msg = "'VarCorr' will be removed in the next version of the package. See help('clickR-deprecated')")
  UseMethod("VarCorr")
}

#' Set header names for word tables
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Internal function for make_word_table. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A flextable object
#' @param args A names list with the header names
#' @importFrom utils tail
#' @return A flextable object with assigned header names
set_noms<-function (x, args){
  .Deprecated(package="clickR", msg = "'set_noms' will be removed in the next version of the package. See help('clickR-deprecated')")
  header_ <- x$header$dataset
  values <- as.list(tail(x$header$dataset, n = 1))
  args <- args[is.element(names(args), x$col_keys)]
  values[names(args)] <- args
  x$header$dataset <- rbind(header_[-nrow(header_), ], as.data.frame(values,
             stringsAsFactors = FALSE, check.names = FALSE))
  x
}

#' Generic function for creating reporting tables
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Generic function for creating reporting tables. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x An compatibleobject that can be summarized
#' @param ... further arguments passed to make_table
#' @return A data frame with the report table
#' @export
#' @examples
#' report(iris)  #Report of descriptive statistics
#' lm1 <- lm(Petal.Length ~ Sepal.Width + Species, data=iris)
#' report(lm1)   #Report of model
report<-function(x, ...){
  .Deprecated(package="clickR", msg = "'report' will be removed in the next version of the package. See help('clickR-deprecated')")
  UseMethod("report")
}

#' Default function for report
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description This is a default function for calling summary(x) on non-implemented classes. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x Any object without specific report function
#' @param ... further arguments passed to summary
#' @return A summary of the object
#' @export
report.default<-function(x, ...){
  .Deprecated(package="clickR", msg = "'report.default' will be removed in the next version of the package. See help('clickR-deprecated')")
  warning(paste("Non-recognized class for report(). Returning summary(", deparse1(substitute(x)), ")", sep=""))
  return(summary(x, ...))
}

#' Report from numeric variable
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A numeric variable
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.numeric<-function(x,...){
  .Deprecated(package="clickR", msg = "'report.numeric' will be removed in the next version of the package. See help('clickR-deprecated')")
  report(data.frame(x), ...)
}

#' Report from categorical variable
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A categorical variable
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.factor<-function(x,...){
  .Deprecated(package="clickR", msg = "'report.factor' will be removed in the next version of the package. See help('clickR-deprecated')")
  report(data.frame(x), ...)
}

#' Report tables of summary data
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a report table ready for publication. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
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
  if(ncol(x) == 0) stop("Data frame has no numeric or factor variables to describe")
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

#' Plot of the coefficients of a model
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a plot of the coefficients of a model. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param coefs A vector with each coefficient
#' @param lwr.int A vector with the lower end of the CI
#' @param upper.int A vector with the upper end of the CI
#' @param offset Y-axis offset for the coefficients
#' @param coefnames Name for each variable
#' @param abline.pos Position for the vertical reference line
#' @param sorted Should the coefficients be sorted from highest to lowest?
#' @param reverse Should the order be reversed?
#' @param pch Type of point
#' @param xlim Limits of the X-axis
#' @param ylim Limits of the Y-axis
#' @param color Color for the points
#' @param ... Further arguments passed to axis()
#' @return A plot of the coefficients with their CI
#' @importFrom graphics abline lines plot.new plot.window points axis
#' @export
#' @examples
#' lm1 <- lm(Petal.Length ~ Sepal.Width + Species, data=iris)
#' a<-report(lm1)
#' par(mar=c(4, 10, 3, 2))
#' #Coefplot calling plot.reportmodel
#' plot(a)
#' #Manual coefplot
#' coefplot(coefs=c(1, 2, 3), lwr.int=c(0, 1, 2), upper.int=c(5, 6, 7), coefnames=c("A", "B", "C"))
coefplot <- function(coefs, lwr.int=coefs, upper.int=coefs, offset=0, coefnames=names(coefs), abline.pos=0, sorted=FALSE, reverse=FALSE, pch=16, xlim=c(min(lwr.int, na.rm=TRUE), max(upper.int, na.rm=TRUE)), ylim=c(1, length(coefs)), color="black", ...){
  .Deprecated(package="clickR", msg = "'coefplot' will be removed in the next version of the package. See help('clickR-deprecated')")
  color <- as.character(data.frame(color, coefs)[,1])
  if(is.null(coefnames)) coefnames <- 1:length(coefs)
  dat <- data.frame(coefs, lwr.int, upper.int, coefnames)
  if(sorted) dat <- dat[order(dat$coefs),]
  if(reverse) dat <- dat[(dim(dat)[1]):1,]
  plot.new()
  plot.window(xlim=xlim, ylim=ylim)
  axis(3, ...)
  axis(2, at=c(1:length(coefs)), las=1, labels=dat$coefnames, lwd=0)
  abline(v=abline.pos, lty=2)
  points(dat$coefs, (1:length(coefs))+offset, pch=pch, col=color)
  for(i in 1:length(coefs)){
    lines(x=c(dat$lwr.int[i], dat$upper.int[i]), y=c(i, i)+offset, col=color[i])
  }
}

#' Coefplot for reportmodel objects
#'
#' @title Deprecated functions in package \sQuote{clickR}
#' @description Creates a coefplot from the reportmodel object. This function will be
#' defunct in the next version. Updated functions are available in \sQuote{repmod} package.
#' @param x A reportmodel object
#' @param ... Further arguments passed to coefplot
#' @export
#' @examples
#' lm1 <- lm(Petal.Length ~ Sepal.Width + Species, data=iris)
#' a<-report(lm1)
#' par(mar=c(4, 10, 3, 2))
#' plot(a)   #Coefplot calling plot.reportmodel
plot.reportmodel<-function(x, ...){
  .Deprecated(package="clickR", msg = "'plot.reportmodel' will be removed in the next version of the package. See help('clickR-deprecated')")
  coefplot(x$coefficients, x$lwr.int, x$upper.int, ...)
}
