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


# Select subsample for tests ----------------------------------------------
#df_A<- df1[grep("A", df1$Class),]
#rm(df1)

# Split in test and training ----------------------------------------------
df1$rows<- rownames(df1)
setkey(df1, "rows")
set.seed(42)
split<- sample(rownames(df1), size=floor(0.6*nrow(df1)))


train1<- df1[split,]
valid1<- df1[!split,]
#rm(df_A)

#nrows<- list(as.integer(c(1:200)), as.integer(c(2:300)))
#train<- train1[1:2000]
#valid<- valid1[2000:3500]



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


#  ------------------------------------------------------------------------


IBS<- NULL
print("Training regularized logit...")
for (i in 1:length(cox.fitform)) {
  
  # displaying model number
  print(paste0("Model ", i, " out of ", length(cox.fitform)))
  
  # training models
  #m.rfs[[1, i]] <-rfsrc(fitform, ntree=rfs.grid$ntree[i], splitrule=rfs.grid$splitrule[i], mtry=rfs.grid$mtry[i], data=train[as.integer(rfs.grid$nrows[[i]])])
  m.cox[[1,i]] <-rfsrc(formula = cox.fitform[[i]],ntree=50, data=train)
  
}


pec.cox<- pec(list("Base"=m.cox[[1,1]], "w/o vendor size"=m.cox[[1,2]], "w/o age"=m.cox[[1,3]], 
                   "w/o Quantile"=m.cox[[1,3]], "w/o DOP"=m.cox[[1,4]], "w/o MS"=m.cox[[1,5]]), 
              formula = Surv(newTOM, status)~ MS + DOP + Quantile + age + size_vendor, data=valid)

plot(pec.cox)



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

