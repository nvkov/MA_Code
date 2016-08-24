#helper functions

##' @S3method predictSurvProb rpart
predictSurvProb.rpart <- function(object,newdata,times,train.data,...){
  -#  require(rpart)
    -  ## require(rms)
    learndat <- train.data
    nclass <- length(unique(object$where))
    learndat$rpartFactor <- factor(predict(object,newdata=train.data,...))
    newdata$rpartFactor <- factor(predict(object,newdata=newdata))
    rpart.form <- reformulate("rpartFactor",eval(object$call$formula)[[2]])  
    -  ##   rpart.form <- reformulate("rpartFactor",object$call$formula[[2]])
      -  #  fit.rpart <- cph(rpart.form,data=learndat,se.fit=FALSE,surv=TRUE,x=TRUE,y=TRUE)
      fit.rpart <- prodlim(rpart.form,data=learndat)
    p <- predictSurvProb(fit.rpart,newdata=newdata,times=times)
    -  #  print(p[100:113,1:10])
      p
}