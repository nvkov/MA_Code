#Plot RFS parameters:

rm(list=ls())
library("data.table")
library("sets")
library("survival")
library("stargazer")
library("beepr")
library("rpart")
library("partykit")
library("coin")
library("caret")
library("verification") # for Brier score
library("pec")
library("survAUC")
library("LTRCtrees")
library("peperr")
library("rms")
library("randomForestSRC")
library("party")
library("ipred")
library("survivalROC")
library("ggplot")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("ready_for_survival.RData")

set.seed(22)

# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor
fitform1<- Surv(newTOM,status)~ MS + DOP + Quantile + age 
fitform2<- Surv(newTOM,status)~ MS + DOP + Quantile +  size_vendor
fitform3<- Surv(newTOM,status)~ MS + DOP +  age + size_vendor
fitform4<- Surv(newTOM,status)~ MS  + Quantile + age + size_vendor
fitform5<- Surv(newTOM,status)~ DOP + Quantile + age + size_vendor


# Select parameters -------------------------------------------------------
cox.fitform<- list(fitform, fitform1, fitform2, fitform3, fitform4, fitform5)

# creating model arrays
m.cox <- array(vector(mode = "list", length = 1), c(2, length(cox.fitform)))

# choose one random sample:
dat<- df1[,i := .I][sample(i, 13500)]
pred<- df1[, i:=.I][sample(i, 1500)]

#  ------------------------------------------------------------------------
IBSrsf<- NULL
IBScforest<- NULL


print("Training models...")
for (i in 1:length(cox.fitform)) {
  
  # displaying model number
  print(paste0("Model ", i, " out of ", length(cox.fitform)))
  
  m.cox[[1,i]] <-rfsrc(formula = cox.fitform[[i]],ntree=100,mtry=2, data=dat)
  m.cox[[2,i]] <-pecCforest(formula = cox.fitform[[i]],control= cforest_control(ntree=100, mtry=2), data=dat)
  
}



pec.rsf<- pec(list("Base"=m.cox[[1,1]], "w/o vendor size"=m.cox[[1,2]], "w/o age"=m.cox[[1,3]], 
                   "w/o Quantile"=m.cox[[1,3]], "w/o DOP"=m.cox[[1,4]], "w/o MS"=m.cox[[1,5]]), 
              formula = Surv(newTOM, status)~ 1, times=c(1:120), data=pred)

print(pec.rsf)


pec.cforest<- pec(list("Base"=m.cox[[2,1]], "w/o vendor size"=m.cox[[2,2]], "w/o age"=m.cox[[2,3]], 
                   "w/o Quantile"=m.cox[[2,3]], "w/o DOP"=m.cox[[2,4]], "w/o MS"=m.cox[[2,5]]), 
              formula = Surv(newTOM, status)~ 1, times=c(1:120), data=pred)

print(pec.cforest)


# Plot prediction error ---------------------------------------------------

plot[c(1:120),]






#  ------------------------------------------------------------------------


# Plot variable importance ------------------------------------------------

model<- rfsrc(fitform, data=train1[1:20000,], ntree=100, mtry=2)
varimp<- vimp(model)
plot(varimp)


varimp<- NULL
for(i in c(1:100)){
  
  print(paste0("Load data for run " , i, " out of 100"))
  load(paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/RSFrun_",i,".RData"))
  
  print(paste0("Calculating variable importance for run ", i))
  varimp[[i]]<- vimp(fitrsf)$importance
  }


varimp<- as.data.frame(varimp)
save(varimp, file="C:/Users/Nk/Documents/Uni/MA/varimp/varimp35.RData")

