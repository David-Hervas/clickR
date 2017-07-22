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
  sx<-summary(x)
  output<-rbind(cbind(round(sx$coefficients[,1],digits)[-c(1:length(x$alpha))],
                      round(sx$coefficients[,2],digits)[-c(1:length(x$alpha))],
                      round(exp(sx$coefficients[,1])[-c(1:length(x$alpha))],digits),
                      round(exp(confint(x)),digits),
                      round(sx$coefficients[,4],digitspvals)[-c(1:length(x$alpha))]),c(round(AIC(x),digits-1),rep("",5)))
  colnames(output)<-c('Estimate','Std. Error','exp(Estimate)','Lower 95%','Upper 95%','P-value')
  rownames(output)[length(rownames(output))]<-c('AIC')
  if(! x$link %in% c("logit")) output[-dim(output)[1],4:5]<-round(confint(x),digits)
  if(! x$link %in% c("logit")) output<-output[,-3]

  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
  }
  return(print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE))
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
  sx<-summary(x)
  output<-rbind(rbind(cbind(round(sx$coefficients[,1],digits)[-c(1:length(x$alpha))],
                            round(sx$coefficients[,2][-c(1:length(x$alpha))],digits),
                            round(exp(sx$coefficients[,1]),digits)[-c(1:length(x$alpha))],
                            rbind(round(exp(confint(x)),digits)[-c(1:length(x$alpha)),]),
                            round(sx$coefficients[,4],digitspvals)[-c(1:length(x$alpha))]),
                      c(round(AIC(x),digits-1),rep("",5))),
                matrix(c(round(rapply(ordinal::VarCorr.clmm(x), function(x) sqrt(diag(x))),digits),
                         rep("",5*length(rapply(ordinal::VarCorr.clmm(x), function(x) sqrt(diag(x)))))),ncol=6,byrow=F))

  colnames(output)<-c('Estimate','Std. Error','exp(Estimate)','Lower 95%','Upper 95%','P-value')
  rownames(output)[length(x$beta)+1]<-c('AIC')
  rownames(output)[rownames(output)==""]<-names(rapply(ordinal::VarCorr.clmm(x), function(x) sqrt(diag(x))))


  if(! x$link %in% c("logit")) output[c(1:(dim(sx$coefficients)[1]-length(x$alpha))),4:5]<-round(confint(x),digits)[-c(1:length(x$alpha)),]
  if(! x$link %in% c("logit")) output<-output[,-3]
  # intervalos log para distinto de logit falta

  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
  }
  return(print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE))
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
  output<-rbind(cbind(round(sx$coefficients[,1, drop=FALSE],digits),round(sx2$coefficients[,2, drop=FALSE],digits),
                      round(sx$coefficients[,2:3, drop=FALSE],digits),round(sx2$coefficients[,4, drop=FALSE],digitspvals)),
                c(round(AIC(x),digits+1),rep("",4)))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[dim(sx$coefficients)[1]+1]<-'AIC'
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
  }
  return(print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE))
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
  sx<-summary(x)
  output<-rbind(cbind(round(sx$coefficients$mean[,1, drop=FALSE],digits),round(sx$coefficients$mean[,2, drop=FALSE],digits),
                      round(confint(x),digits)[-dim(confint(x))[1], ,drop=FALSE],round(sx$coefficients$mean[,4, drop=FALSE],digitspvals)),
                c(round(sx$coefficients$precision[1],digits+1),rep("",4)),
                c(round(sx$pseudo.r.squared, digits+1), rep("", 4)))
  colnames(output)<-c('Estimate','Std. Error','Lower 95%','Upper 95%','P-value')
  rownames(output)[c(dim(sx$coefficients$mean)[1]+1, dim(sx$coefficients$mean)[1]+2)]<-c("phi", "Pseudo R-squared")
  output[,"P-value"][output[,"P-value"]=="0"]<-"<0.001"
  if(!is.null(file)){
    make_table(output, file, type, font, pointsize)
  }
  return(print(data.frame(output, check.names=FALSE, stringsAsFactors=FALSE), row.names=TRUE, right=TRUE))
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

