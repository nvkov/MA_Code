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
df_A<- df1[grep("A", df1$Class),]
rm(df1)

# Split in test and training ----------------------------------------------
df_A$rows<- rownames(df_A)
setkey(df_A, "rows")
set.seed(42)
split<- sample(rownames(df_A), size=floor(0.6*nrow(df_A)))


train1<- df_A[split,]
valid1<- df_A[!split,]
rm(df_A)

nrows<- list(as.integer(c(1:200)), as.integer(c(2:300)))
train<- train1[1:2000]
valid<- valid1[2000:30000]



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

#define validation sets:
d<- c(1:nrow(valid))
validationsets<- as.list(split(d, ceiling(seq_along(d)/1500)))


m.cox=NULL


#  ------------------------------------------------------------------------


IBS<- NULL
print("Training regularized logit...")
for (i in 1:length(cox.fitform)){
  
  # displaying model number
  print(paste0("Model ", i, " out of ", length(cox.fitform)))
  
  # training models
    m.cox[[1,i]] <-pecCforest(formula = cox.fitform[[i]], control=cforest_control(ntree=50), data=train)
  }  


Info<- NULL

for(j in 1:length(validationsets)){
  
  # displaying model number
  print(paste0("Model ", j, " out of ", length(validationsets)))
  
pec.cox<- pec(list("Base"=m.cox[[1,1]], "w/o vendor size"=m.cox[[1,2]], "w/o age"=m.cox[[1,3]], 
                   "w/o Quantile"=m.cox[[1,3]], "w/o DOP"=m.cox[[1,4]], "w/o MS"=m.cox[[1,5]]), 
              formula = Surv(newTOM, status)~ MS + DOP + Quantile + age + size_vendor, data=valid[as.integer(validationsets[[j]])])

Info[[j]]<- pec.cox
}



cindex.cox<- cindex(list("Base"=m.cox[[1,1]], "w/o vendor size"=m.cox[[1,2]], "w/o age"=m.cox[[1,3]], 
                   "w/o Quantile"=m.cox[[1,3]], "w/o DOP"=m.cox[[1,4]], "w/o MS"=m.cox[[1,5]]), 
              formula = Surv(newTOM, status)~ MS + DOP + Quantile + age + size_vendor,eval.times = c(1:60), data=valid)

plot(cindex.cox)