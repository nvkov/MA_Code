# Predicting probabilities from the models:

#Survival analysis:

#Correct Leistung:

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
library("rms")
library("randomForestSRC")
library("party")
library("peperr")


#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("ready_for_survival.RData")

# Split in test and training ----------------------------------------------
df1$rows<- rownames(df1)
setkey(df1, "rows")
set.seed(42)
split<- sample(rownames(df1), size=floor(0.6*nrow(df1)))


#train<- df1[split,]
valid<- df1[!split,]
rm(df1)



#  ------------------------------------------------------------------------

# Create grid for bootstrap -----------------------------------------------

d<- c(1:nrow(valid))
valid.nrows<- as.list(split(d, ceiling(seq_along(d)/1500))) 

# creating parameter grids
data.grid <- expand.grid(nrow=data.nrows) 

BS<- NULL
CI<- NULL

BSrpart<- NULL
BSReference<- NULL
BSCox<- NULL
BScforest<- NULL
BSrsf<- NULL
BSctree<- NULL

BSrpart.temp<- NULL
BSReference.temp<- NULL
BSCox.temp<- NULL
BScforest.temp<- NULL
BSrsf.temp<- NULL
BSctree.temp<- NULL


CIrpart<- NULL
CICox<- NULL
CIcforest<- NULL
CIrsf<- NULL
CIctree<- NULL


CIrpart.temp<- NULL
CICox.temp<- NULL
CIcforest.temp<- NULL
CIrsf.temp<- NULL
CIctree.temp<- NULL


time_beginning<- proc.time()

for(i in c(1:100)){
  
  print(paste0("Load data for run " , i, " out of 100"))
  load(paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/Rpartrun_",i,".RData"))
  load(paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/Ctree_",i,".RData"))
  load(paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/RSFrun_",i,".RData"))
  load(paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/Cforestrun_",i,".RData"))
  load(paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/Coxrun_",i,".RData"))

  for(j in c(25)){
    print("-----------------------------------------------")
    print(paste0("Calculate for validation set ", j, " out of 600"))
    dat<- valid[as.integer(valid.nrows[[j]])]
    
    print("Brier score")
    BStemp <- pec(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart, "ctree"=fitctree), 
                 formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=dat, times=c(1:120))
   
    BSrpart.temp[[j]]<- BStemp$AppErr$rpart[1:120]
    BSrsf.temp[[j]]<- BStemp$AppErr$rsf[1:120]
    BScforest.temp[[j]]<- BStemp$AppErr$cforest[1:120]
    BSCox.temp[[j]]<- BStemp$AppErr$Cox[1:120]
    BSReference.temp[[j]]<- BStemp$AppErr$Reference[1:120]
    BSctree.temp[[j]]<- BStemp$AppErr$ctree[1:120]
   
    print("C-Index")
  
  #Concordance index:
    CItemp<- cindex(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart, "ctree"=fitctree), 
                  formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=dat, eval.times=c(1:120))
  
    CIrpart.temp[[j]]<- CItemp$AppCindex$rpart[1:120]
    CIrsf[[j]]<- CItemp$AppCindex$rsf[1:120]
    CIcforest[[j]]<- CItemp$AppCindex$cforest[1:120]
    CICox[[j]]<- CItemp$AppCindex$Cox[1:120]
    CIctree[[j]]<- CItemp$AppCindex$ctree[1:120]
  
    print("Finished calculation")
    }

  print("Save BS")
  
  BSrpart[[i]]<- BSrpart.temp
  BSrsf[[i]]<- BSrsf.temp
  BScforest[[i]]<- BScforest.temp
  BSCox[[i]]<- BSCox.temp
  BSctree[[i]]<- BSctree.temp
  

  save(BSrpart, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/BSrpart_valid25.RData")
  save(BSrsf, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/BSrsf_valid25.RData")
  save(BScforest, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/BScforest_valid25.RData")
  save(BSCox, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/BScox_valid25.RData")
  save(BSctree, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/BSctree_valid25.RData")
  
  
    
  print("Save CI")
  
  CIrpart[[i]]<- (CIrpart.temp)
  CIrsf[[i]]<- (CIrsf.temp)
  CIcforest[[i]]<- (CIcforest.temp)
  CICox[[i]]<- (CICox.temp)
  CIctree[[i]]<- (CIctree.temp)
  
  save(CIrpart, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/CIraprt_valid25.RData")
  save(CIrsf, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/CIrsf_valid25.RData")
  save(CIcforest, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/CIcforest_valid25.RData")
  save(CICox, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/CIcox_valid25.RData")
  save(CIctree, file="C:/Users/Nk/Documents/Uni/MA/results_ensembles/CIctree_valid25.RData")
  
}
  
time_end<- proc.time()  

#------------------------------------------------------------
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

## Recursively step down into list, removing all such objects 
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

  
  

