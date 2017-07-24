#' Report from linear model
#'
#' @description Creates a report table from a linear model
#' @param x A linear model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.lm<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                    font=ifelse(Sys.info()["sysname"]=="Windows", "Arial", "Helvetica")[[1]],
                    pointsize=11, ...){
  sx <- summary(x)
  ci <- confint(x)
  obj <- list(coefficients=sx$coefficients[,1], se=sx$coefficients[,2], lwr.int=ci[,1],
              upper.int=ci[,2], pvalues=sx$coefficients[,4], r.squared=sx$r.squared, adj.r.squared=sx$adj.r.squared)
  output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),
                      round(obj$lwr.int,digits),round(obj$upper.int, digits), round(obj$pvalues,digitspvals)),
                c(round(obj$r.squared,digits+1),rep("",4)),
                c(round(obj$adj.r.squared,digits+1),rep("",4)))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[c(dim(sx$coefficients)[1]+1,dim(sx$coefficients)[1]+2)]<-c('R Squared','Adj.R Squared')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.glm<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                     font=ifelse(Sys.info()["sysname"]=="Windows", "Arial", "Helvetica")[[1]],
                     pointsize=11, ...){
  compute.exp<-x$family$link %in% c("logit", "log")
  sx<-summary(x)
  ci<-confint(x)
  obj <- list(coefficients=sx$coefficients[,1], se=sx$coefficients[,2], lwr.int=ci[,1],
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
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.coxph<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                       font=ifelse(Sys.info()["sysname"]=="Windows", "Arial", "Helvetica")[[1]],
                       pointsize=11, ...){
  sx<-summary(x)
  obj <- list(coefficients=sx$coefficients[,1], se=sx$coefficients[,3], hr=sx$conf.int[,1],
              lwr.int=sx$conf.int[,3], upper.int=sx$conf.int[,4], pvalues=sx$coefficients[,5], aic=AIC(x))
  output<-rbind(cbind(round(obj$coefficients,digits), round(obj$se, digits), round(obj$hr, digits),
                      round(obj$lwr.int,digits), round(obj$upper.int, digits),
                      round(obj$pvalues,digitspvals)),c(round(obj$aic,digits),rep('',5)))
  colnames(output)<-c('Estimate','Std. Error','HR','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(output)[1]]<-c('AIC')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.merModLmerTest<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                                font=ifelse(Sys.info()["sysname"] == "Windows", "Arial",
                                            "Helvetica")[[1]], pointsize=11, ...){
  sx=lmerTest::summary(x)
  cor<-as.data.frame(lme4::VarCorr(x))
  ci <- confint(x)
  #cor[dim(cor)[1],2]<-'Residual'
  obj<- list(coefficients=sx$coefficients[,1], se=sx$coefficients[,2], lwr.int=ci[,1][-c(1:dim(as.data.frame(lme4::VarCorr(x)))[1])],
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
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.lmerMod<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                         font=ifelse(Sys.info()["sysname"] == "Windows", "Arial",
                                     "Helvetica")[[1]], pointsize=11, ...){
  x<-lmerTest::lmer(x@call,data=x@frame)
  report.merModLmerTest(x, file, type, digits, digitspvals, font, pointsize)
}

#' Report from generalized linear mixed model
#'
#' @description Creates a report table from a generalized linear mixed model
#' @param x A generalized linear mixed model object
#' @param file Name of the file to export the table
#' @param type Format of the file
#' @param digits Number of decimals
#' @param digitspvals Number of decimals for p-values
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.glmerMod<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                          font=ifelse(Sys.info()["sysname"] == "Windows", "Arial",
                                      "Helvetica")[[1]], pointsize=11, ...){
  compute.exp<-x@resp$family$link %in% c("logit", "log")
  sx<-summary(x)
  cor<-as.data.frame(lme4::VarCorr(x))
  ci <- confint(x)
  obj<- list(coefficients=sx$coefficients[,1], se=sx$coefficients[,2], lwr.int=ci[,1][-c(1:dim(as.data.frame(lme4::VarCorr(x)))[1])],
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
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.lqmm<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                      font=ifelse(Sys.info()["sysname"] == "Windows", "Arial",
                                  "Helvetica")[[1]], pointsize=11, ...){
  sx<-lqmm::summary.lqmm(x, ...)
  obj<-list(coefficients=sx$tTable[,1], se=sx$tTable[,2], lwr.int=sx$tTable[,3], upper.int=sx$tTable[,4],
            pvalues=sx$tTable[,5], aic=sx$aic, random=round(lqmm::VarCorr.lqmm(x)))
  output<-rbind(rbind(cbind(round(obj$coefficients,digits), round(obj$se,digits),
                            round(obj$lwr.int, digits), round(obj$upper.int, digits), round(obj$pvalues, digitspvals)),
                      c(round(obj$aic, digits),rep("",4))),
                matrix(c(round(obj$random,digits),rep("",4*length(obj$random))),ncol=5,byrow=F))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$tTable)[1]+1]<-c('AIC')
  rownames(output)[(dim(sx$tTable)[1]+2):(((dim(sx$tTable)[1]+2)+length(obj$random))-1)]<-paste('Ran.Eff',names(lqmm::VarCorr.lqmm(x)),sep=' ')
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.clm<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                     font=ifelse(Sys.info()["sysname"] == "Windows", "Arial",
                                 "Helvetica")[[1]], pointsize=11, ...){

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
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.clmm<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                      font=ifelse(Sys.info()["sysname"] == "Windows", "Arial",
                                  "Helvetica")[[1]], pointsize=11, ...){
  compute.exp<-x$link %in% c("logit", "log")
  sx<-summary(x)
  ci<-confint(x)
  obj <- list(coefficients=sx$coefficients[,1][-c(1:length(x$alpha))], se=sx$coefficients[,2][-c(1:length(x$alpha))],
              lwr.int=ci[,1][-c(1:length(x$alpha))], upper.int=ci[,2][-c(1:length(x$alpha))], pvalues=sx$coefficients[,4][-c(1:length(x$alpha))], aic=AIC(x),
              random=ordinal::VarCorr.clmm(x))
  if(compute.exp){
    obj$exp.coef <- exp(obj$coefficients)
    obj$exp.lwr.int <- exp(obj$lwr.int)
    obj$exp.upper.int <- exp(obj$upper.int)
  }
  obj$thresholds <- sx$coefficients[1:length(x$alpha),]
  output<-rbind(rbind(cbind(round(obj$coefficients,digits), round(obj$se, digits),
                            if(compute.exp) {
                              cbind(round(obj$exp.coef,digits), round(obj$exp.lwr.int, digits),
                                    round(obj$upper.int, digits))
                            } else{
                              cbind(round(obj$lwr.int, digits), round(obj$upper.int, digits))
                            }
                             , round(obj$pvalues,digitspvals)), c(round(obj$aic,digits),rep("",ifelse(compute.exp, 5, 4)))),
                matrix(c(round(rapply(obj$random, function(x) sqrt(diag(x))),digits),
                         rep("",ifelse(compute.exp, 5, 4)*length(rapply(obj$random, function(x) sqrt(diag(x)))))),ncol=ifelse(compute.exp, 6, 5),byrow=F))
  colnames(output)<-c('Estimate','Std. Error', if(compute.exp) 'exp(Estimate)','Lower 95%','Upper 95%','P-value')
  rownames(output)[length(x$beta)+1]<-c('AIC')
  rownames(output)[rownames(output)==""]<-names(rapply(ordinal::VarCorr.clmm(x), function(x) sqrt(diag(x))))
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.rq<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                    font=ifelse(Sys.info()["sysname"]=="Windows", "Arial", "Helvetica")[[1]],
                    pointsize=11, ...){
  sx<-summary(x)
  sx2<-summary(x, covariance=TRUE)
  obj<-list(coefficients=sx$coefficients[,1], se=sx2$coefficients[,2], lwr.int=sx$coefficients[,3],
            upper.int=sx$coefficients[,4], pvalues=sx2$coefficients[,4], aic=AIC(x))
  output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),
                      round(obj$lwr.int,digits), round(obj$upper.int, digits),
                      round(obj$pvalues,digitspvals)),
                c(round(obj$aic, digits),rep("",4)))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$coefficients)[1]+1]<-'AIC'
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.betareg<-function(x, file=NULL, type="word", digits=3, digitspvals=3,
                         font=ifelse(Sys.info()["sysname"]=="Windows", "Arial", "Helvetica")[[1]],
                         pointsize=11, ...){
  compute.exp<-x$link$mean$name %in% c("logit", "log")
  sx<-summary(x)
  ci<-confint(x)
  obj<-list(coefficients=sx$coefficients$mean[,1], se=sx$coefficients$mean[,2], lwr.int=ci[,1][-dim(ci)[1]],
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
    make_table(output, file, type, font, pointsize)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.brmsfit<-function(x, file=NULL, type="word", digits=3,
                         font=ifelse(Sys.info()["sysname"]=="Windows", "Arial", "Helvetica")[[1]],
                         pointsize=11, ...){
  compute.exp<-x$family$link %in% c("logit", "log")
  sx<-summary(x)
  random<-tryCatch(do.call(rbind, sx$random), error=function(e) NA)
  if(!is.na(random[1])) rownames(random)<-paste("Sd", names(sx$random), sep=" ")
  obj<-list(coefficients=sx$fixed[,1], se=sx$fixed[,2], lwr.int=sx$fixed[,3],
            upper.int=sx$fixed[,4], random=random)
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
                      }), if(!is.na(random[1])) {cbind(round(random[,1:2, drop=FALSE], digits), if(compute.exp) "-", round(random[,3:4, drop=FALSE], digits))})
  colnames(output)<-c('Estimate','Std. Error',if(compute.exp) 'exp(Estimate)', 'Lower 95%','Upper 95%')
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
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
#' @param font Font to use if type="word"
#' @param pointsize Pointsize to use if type="word"
#' @param ... Further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report.glmnet<-function(x, s, drop.zero=TRUE, file=NULL, type="word", digits=3,
                         font=ifelse(Sys.info()["sysname"]=="Windows", "Arial", "Helvetica")[[1]],
                         pointsize=11, ...){
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
    make_table(output, file, type, font, pointsize)
  }
  print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE)
  class(obj) <- "reportmodel"
  invisible(obj)
}



#' Export a table to word
#'
#' @description Exports a table to Word
#' @param x A data.frame object
#' @param pointsize Font size
#' @param font Font type
#' @param file Name of the file
#' @param add.rownames Should rownames be added to the output?
#' @return Creates a word with the table
#' @export
make_word_table <- function(x, pointsize, font, file, add.rownames){
  mydoc <- ReporteRs::docx()
  MyFTable= ReporteRs::FlexTable(data.frame(x, check.names=FALSE, stringsAsFactors=FALSE),
                                 add.rownames=add.rownames,header.text.props=ReporteRs::textProperties(font.size=pointsize,
                                                                                                       font.weight = 'bold',font.family = font),
                                 body.text.props=ReporteRs::textProperties(font.family = font,font.size=pointsize),
                                 body.par.props = ReporteRs::parProperties(text.align='center'))
  MyFTable = ReporteRs::setFlexTableBorders(MyFTable,
                                            inner.vertical = ReporteRs::borderProperties( style = "none" ),
                                            inner.horizontal = ReporteRs::borderProperties( style = "none"),
                                            outer.vertical = ReporteRs::borderProperties( style='none' ),
                                            outer.horizontal = ReporteRs::borderProperties( width = 3 ) )
  mydoc = ReporteRs::addFlexTable(mydoc,MyFTable)
  ReporteRs::writeDoc(mydoc, file = paste(file, ".docx", sep=""))
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
#' @return Creates a .csv file with the table
#' @export
make_csv_table <- function(x, file){
  write.csv2(data.frame(x, check.names=FALSE, stringsAsFactors=FALSE), paste(file, ".csv", sep=""),
             row.names=TRUE)
}


#' Make a table from report
#'
#' @description Auxiliary function to create tables
#' @param x A data.frame object
#' @param file Name of the file
#' @param type Type of file
#' @param font Font type
#' @param pointsize Size of font
#' @param add.rownames Should rownames be added to the output?
#' @return Creates a file with the table
#' @export
make_table<-function(x, file, type, font="Arial", pointsize=11, add.rownames=TRUE){
  if(type=="csv") {make_csv_table(x, file)}
  if(type=="latex") {make_latex_table(x, file)}
  if(is.null(type) | type=="word") {make_word_table(x, pointsize, font, file, add.rownames = add.rownames)}
  message(paste0("Exported table as ", file))
}


#' Generic function for reporting of models
#'
#' @description Generic function for reporting of models
#' @param x A model object
#' @param ... further arguments passed to make_table
#' @return A data frame with the report table
#' @export
report<-function(x, ...){
  UseMethod("report")
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

