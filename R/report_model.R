#' Report from linear model
#'
#' @description Creates a report table from a linear model
#' @param x A linear model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats confint getCall
#' @export
report.lm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from generalized linear model
#'
#' @description Creates a report table from a generalized linear model
#' @param x A generalized linear model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.glm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from cox regression model
#'
#' @description Creates a report table from a cox model
#' @param x A cox model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats AIC getCall
#' @export
report.coxph<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}



#' Report from linear mixed model with pvalues
#'
#' @description Creates a report table from a linear mixed model
#' @param x A linear mixed model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @importFrom methods loadMethod
#' @export
report.merModLmerTest<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from linear mixed model
#'
#' @description Creates a report table from a linear mixed model
#' @param x A linear mixed model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.lmerMod<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
  x<-lmerTest::lmer(x@call,data=x@frame)
  report.merModLmerTest(x, file, type, digits, digitspvals, info=info, ...)
}

#' Report from generalized linear mixed model
#'
#' @description Creates a report table from a generalized linear mixed model
#' @param x A generalized linear mixed model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.glmerMod<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from quantile mixed model
#'
#' @description Creates a report table from a quantile mixed model
#' @param x A quantile model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.lqmm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from ordinal model
#'
#' @description Creates a report table from an ordinal model
#' @param x An ordinal model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.clm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){

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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Report from ordinal mixed model
#'
#' @description Creates a report table from an ordinal mixed model
#' @param x An ordinal model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.clmm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from quantile regression model
#'
#' @description Creates a report table from a quantile regression model
#' @param x A quantreg model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.rq<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from beta regression model
#'
#' @description Creates a report table from a beta regression model
#' @param x A betareg model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.betareg<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report models from brms package
#'
#' @description Creates a report table from model fitted by brms
#' @param x A brms model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.brmsfit<-function(x, file=NULL, type="word", digits=3, info=TRUE, ...){
  compute.exp<-x$family$link %in% c("logit", "log")
  sx<-summary(x)
  WC<-eval(parse(text="brms::WAIC(x)"))
  random<-tryCatch(do.call(rbind, sx$random), error=function(e) NA)
  if(!any(is.na(random))) rownames(random)<-paste(rownames(random),rep(names(sx$random), sapply(sx$random, nrow)), sep=" ")
  obj<-list(coefficients=setNames(sx$fixed[,1], rownames(sx$fixed)), se=sx$fixed[,2], lwr.int=sx$fixed[,3],
            upper.int=sx$fixed[,4], random=random, WAIC=setNames(c(WC$estimates[3,1], WC$estimates[3,2]), c("WAIC", "WAIC SE")), Eff.Sample_min=round(min(sx$fixed[,5])), Rhat_max=round(max(sx$fixed[,6]),2))
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  if(obj$Rhat_max > 1.1) warning("Please diagnose your model, Rhat values greater than 1.1")
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report models from glmnet package
#'
#' @description Creates a report table from models fitted by glmnet
#' @param x A glmnet model object
#' @param s Value of lambda for estimating the coefficients
#' @param drop.zero Should zero coefficients be dropped?
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats coef getCall
#' @export
report.glmnet<-function(x, s, drop.zero=TRUE, file=NULL, type="word", digits=3, info=TRUE, ...){
  compute.exp<- any(grepl("binomial|cox", x$call))
  coefs <- coef(x, s=s)
  obj <- list(coefficients=as.numeric(coefs)[if(drop.zero) {as.numeric(coefs)!=0}], lwr.int=NA, upper.int=NA)
  names(obj$coefficients)<-rownames(coefs)[if(drop.zero) {as.numeric(coefs)!=0}]
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
  }
  obj$lambda <- s
  output<-rbind(cbind(round(obj$coefficients,digits),
                if(compute.exp){
                  round(obj$exp.coef, digits)
                }), cbind(s, rep("", ifelse(compute.exp, 1, 0))))
  colnames(output)<-c('Estimate', if(compute.exp) 'exp(Estimate)')
  rownames(output)<-c(names(obj$coefficients), "lambda")
  if(!is.null(file)){
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from robust linear model (rlm)
#'
#' @description Creates a report table from a robust linear model
#' @param x A rlm object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.rlm<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}


#' Report from generalized linear mixed model from ADMB
#'
#' @description Creates a report table from a glmmadmb model
#' @param x A generalized linear mixed model object (glmmabmb)
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values


#' @param info If TRUE, include call in the exported table
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @importFrom stats getCall
#' @export
report.glmmadmb<-function(x, file=NULL, type="word", digits=3, digitspvals=3, info=TRUE, ...){
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
    info <- if(info) deparse(getCall(x)) else NULL
    make_table(output, file, type, info=info, ...)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}

#' Function to compute p-values for robust linear regression models
#'
#' @description Estimates p-values for rlm models
#' @param x A rlm object
#' @return A vector of p-values
#' @importFrom stats pf
#' @export
rob.pvals <- function(x){
  coefs <- x$coef
  sx<-summary(x, method = "XtWX")
  covs<-diag(sx$cov.unscaled)*sx$stddev^2
  statistics<-sapply(1:length(coefs), function(x) sum(coefs[x]*solve(covs[x], coefs[x])))
  pf(statistics, 1, sx$df[2], lower.tail = FALSE)
}

#' Function to compute bootstrap confidence intervals for robust linear regression models
#'
#' @description Estimates confidence intervals for rlm models
#' @param x A rlm object
#' @param level Confidence level for the interval
#' @param maxit Maximum number of iterations per fit
#' @param R Number of boostrap samples
#' @return A matrix with bootstrap confidence intervals for each variable in the model
#' @importFrom stats formula
#' @export
rob.ci <- function(x, level=0.95, maxit=200, R=2000){
  coefb <- function(object, data, indices){      #Función para extraer el R2 de un modelo
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
#' @description Exports a table to Word
#' @param x A data.frame object
#' @param file Name of the file
#' @param info Footer for the table
#' @param use.rownames Should row names be added to the output?
#' @return Creates a word file with the table
#' @export
make_word_table <- function(x, file, info=NULL, use.rownames=TRUE){
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
#' @description Exports a table to latex
#' @param x A data.frame object
#' @param file Name of the file
#' @return Creates a .txt file with latex code for the table
#' @export
make_latex_table <- function(x, file){
  print(xtable::xtable(x), file=paste(file, ".txt", sep=""))
}

#' Export a table to excel
#'
#' @description Exports a table to Excel
#' @param x A data.frame object
#' @param file Name of the file
#' @param info Footer for the table
#' @importFrom utils write.csv2
#' @return Creates a .csv file with the table
#' @export
make_csv_table <- function(x, file, info){
  write.csv2(data.frame(rbind(x, c(info, rep("", ncol(x)-1))), check.names=FALSE,
                        stringsAsFactors=FALSE), paste(file, ".csv", sep=""),
             row.names=TRUE)
}


#' Make a table from report
#'
#' @description Auxiliary function to create tables
#' @param x A data.frame object
#' @param file Name of the file
#' @param type Type of file
#' @param info Footer for the table
#' @param ... Additional parameters passed to make_word_table
#' @return Creates a file with the table
#' @export
make_table<-function(x, file, type, info=NULL, ...){
  if(type=="csv") {make_csv_table(x, file, info)}
  if(type=="latex") {make_latex_table(x, file)}
  if(is.null(type) | type=="word") {make_word_table(x, file, info, ...)}
  message(paste0("Exported table as ", file))
}


#' Generic function for reporting of models
#'
#' @description Generic function for reporting of models
#' @param x A model object
#' @param ... further arguments passed to make_table
#' @return A data frame with the report table
#' @export
#' @examples
#' report(iris)  #Report of descriptive statistics
#' lm1 <- lm(Petal.Length ~ Sepal.Width + Species, data=iris)
#' report(lm1)   #Report of model
report<-function(x, ...){
  UseMethod("report")
}

#' Generic VarCorr function
#'
#' @description Extract Variance-Covariance Matrix
#' @param x A model object
#' @param sigma Optional value used as a multiplier for the standard deviations
#' @param ... Further arguments passed to VarrCorr methods
#' @return A Variance-Covariance Matrix
#' @export
VarCorr<-function (x, sigma = 1, ...){
  UseMethod("VarCorr")
}

#' Default function for report
#'
#' @description This is a default function for calling summary(x) on non-implemented classes
#' @param x Any object without specific report function
#' @param ... further arguments passed to summary
#' @return A summary of the object
#' @export
report.default<-function(x, ...){
  warning(paste("Non-recognized class for report(). Returning summary(", deparse(substitute(x)), ")", sep=""))
  return(summary(x, ...))
}


#' Report from numeric variable
#'
#' @description Creates a report table
#' @param x A numeric variable
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.numeric<-function(x,...){
  report(data.frame(x), ...)
}

#' Report from categorical variable
#'
#' @description Creates a report table
#' @param x A categorical variable
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.factor<-function(x,...){
  report(data.frame(x), ...)
}


#' Set header names for word tables
#'
#' @description Internal function for make_word_table
#' @param x A flextable object
#' @param args A names list with the header names
#' @importFrom utils tail
#' @return A flextable object with assigned header names
set_noms<-function (x, args)
{
  header_ <- x$header$dataset
  values <- as.list(tail(x$header$dataset, n = 1))
  args <- args[is.element(names(args), x$col_keys)]
  values[names(args)] <- args
  x$header$dataset <- rbind(header_[-nrow(header_), ], as.data.frame(values,
             stringsAsFactors = FALSE, check.names = FALSE))
  x
}
