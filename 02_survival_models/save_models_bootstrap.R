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


train<- df1[split,]
valid<- df1[!split,]
rm(df1)

#train<- train[30000:145000,]
valid<- valid[3000:4500]
#rm(df1)

#  ------------------------------------------------------------------------

# Create grid for bootstrap -----------------------------------------------

d<- c(1:nrow(train))
data.nrows<- as.list(split(d, ceiling(seq_along(d)/13500))) 

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


CIrpart<- NULL
CICox<- NULL
CIcforest<- NULL
CIrsf<- NULL
CIctree<- NULL


#i<-1
for(i in c(1:11)){
  print(paste0("Model ", i, " out of ", length(data.nrows)))
  dat<- train[as.integer(data.nrows[[i]])]
  
fitform <- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor 


fitcox <- selectCox(fitform, data=dat, rule="aic")
save(fitcox, file=paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/Coxrun_",i,".RData"))


fitrpart<- pecRpart(fitform, data=dat)
save(fitrpart, file=paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/Rpartrun_",i,".RData"))

set.seed(13)

fitrsf <- rfsrc(fitform,data=dat,forest=TRUE,ntree=100, mtry=2)
save(fitrsf, file=paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/RSFrun_",i,".RData"))

set.seed(13)

fitctree<- pecCtree(fitform, data=dat)
save(fitctree, file=paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/Ctreerun_",i,".RData"))


fitcforest <- pecCforest(fitform, data=dat, controls=cforest_classical(ntree=100, mtry=2))
save(fitcforest, file=paste0("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/saved_models/Cforestrun_",i,".RData"))

extends <- function(...)TRUE

set.seed(2006)

# print(paste0("Predict and calculate performance measures for run ", i, " out of ", length(data.nrows)))
# #Integrated Brier Score:
# print("Brier Score")
# 
# BStemp <- pec(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart, "ctree"=fitctree), 
#               formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid[1:1000], times=c(1:120))
# 
# BSrpart[[i]]<- BStemp$AppErr$rpart
# BSrsf[[i]]<- BStemp$AppErr$rsf
# BScforest[[i]]<- BStemp$AppErr$cforest
# BSCox[[i]]<- BStemp$AppErr$Cox
# BSReference[[i]]<- BStemp$AppErr$Reference
# BSctree[[i]]<- BStemp$AppErr$ctree
# 
# print("C-Index")
# 
# #Concordance index:
# CItemp<- cindex(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart, "ctree"=fitctree), 
#             formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid[1:1000], eval.times=c(1:120))
# 
# CIrpart[[i]]<- CItemp$AppCindex$rpart
# CIrsf[[i]]<- CItemp$AppCindex$rsf
# CIcforest[[i]]<- CItemp$AppCindex$cforest
# CICox[[i]]<- CItemp$AppCindex$Cox
# CIctree[[i]]<- CItemp$AppCindex$ctree

}


BSrpart<- as.data.frame(BSrpart)
meanRpart<- apply(BSrpart, 1, mean)
sdRpart<- apply(BSrpart, 1, sd)
ulRpart<- meanRpart + 1.98*sdRpart

BSrsf<- as.data.frame(BSrsf)
meanRsf<- apply(BSrsf, 1, mean)
sdRsf<- apply(BSrsf, 1, sd)

BSctree<- as.data.frame(BSctree)
meanCtree<- apply(BSctree, 1, mean)
sdCtree<- apply(BSctree, 1, sd)

BSCox<- as.data.frame(BSCox)
meanCox<- apply(BSCox, 1, mean)
sdCox<- apply(BSCox, 1, sd)


BSCforest<- as.data.frame(BScforest)
meanCforest<- apply(BSCforest, 1, mean)
sdCforest<- apply(BSCforest, 1, sd)


plot(meanRsf, ylim=c(0,0.3), type="l")
lines(I(meanRsf+1.98*sdRsf), col="black", lty=2)
lines(I(meanRsf-1.98*sdRsf), col="black", lty=2)


lines(I(meanRpart+1.98*sdRpart), col="red", lty=2)
lines(I(meanRpart-1.98*sdRpart), col="red", lty=2)

lines(meanRpart, col="red")
lines(meanCtree, col="blue")

lines(I(meanCtree+1.98*sdCtree), col="blue", lty=2)
lines(I(meanCtree-1.98*sdCtree), col="blue", lty=2)


lines(meanCox, col="pink")
lines(I(meanCox+1.98*sdCox), col="pink", lty=2)
lines(I(meanCox-1.98*sdCox), col="pink", lty=2)


lines(meanCforest, col="green")
lines(I(meanCforest+1.98*sdCforest), col="green", lty=2)
lines(I(meanCforest-1.98*sdCforest), col="green", lty=2)

legend(150, 0.25, c("RSF", "Cforest", "Cox", "Rpart", "Ctree"), col=c("black", "green", "pink", "red", "blue"),
       lwd=c(2,2,2,2,2))

