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
library("peperr")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("dataset_full.RData")

df$newTOM<- as.numeric(df$prices_lastDate-df$prices_firstDate) +1
df$status<- abs(df$right_censor - 1)
df$year_bought<-as.factor(format(df$prices_lastDate, "%Y")) 

# Survival models ---------------------------------------------------------
relCols<- c("valuePrice", "newTOM", "MS","DOP", "Quantile", "age", "Leather_seats", 
            "Full_service_history", "Xenon_lights", "color_cat", "year_bought", 
            "Leistung", "Typ","status", "vendor_ID", "car_ID", "prices_firstDate", "prices_lastDate", "Kategorie") 
df1<- df[df$DOP<=2 & df$newTOM<400,relCols, with=F]
df1$newDOP<- df1$DOP*10
df1$age<- df1$age/30
# Generate some vendor profile variables ----------------------------------

df1<- df1[ ,size_vendor:=length(unique(car_ID)), by=vendor_ID]
df1$size_vendor<- df1$size_vendor/100

df1<- df1[,times_present_diff_price:=1:.N, by=car_ID]
df1<- d1[, total_price_reduction]
df1$valuePrice100<- df1$valuePrice/100
df1$Class<- gsub("[0-9]", "" ,df1$Typ)
df1$Hub_Cat<- gsub("[A-Z]", "", df1$Typ)
# Begin survivavl analysis ------------------------------------------------


# # Kaplan-Meier vs. Nelson-Aalen -------------------------------------------

# Cox proportional hazards ------------------------------------------------

regressors<-c("MS","DOP", "Quantile", "age", 
              "Leather_seats", "Full_service_history",
              "Xenon_lights", "color_cat", "year_bought")

regs<- "MS+ DOP + Quantile + age + 
Leather_seats +Full_service_history +
Xenon_lights + color_cat + year_bought"

rm(df)
df1$newMS<- df1$MS/10
# Survival models ---------------------------------------------------------
Models<- NULL
Models1<- NULL
#for (i in unique(factor(df1$Typ))){
i<- "A140"
  Models[[i]]<- rpart(Surv(newTOM, status) ~ MS + DOP + Quantile + age + 
                         color_cat +size_vendor, data=df1[df1$Typ==i])

  Models1[[i]]<- ctree(Surv(newTOM, status) ~  MS+ DOP + Quantile + age +as.factor(color_cat), data=df1[df1$Typ==i], mincriterion=0.999999)
  #}

plot(Models[[i]])
text(Models[[i]])
  
  
plot(Models1[[i]])
text(Models1[[i]])

df_A<- df1[df1$Class=="A"]

# i<- "A"
# Models[[i]]<- rpart(Surv(newTOM, status) ~ MS + DOP + Quantile + age + 
#                       color_cat +size_vendor, data=df_A)
# 
# Models1[[i]]<- ctree(Surv(newTOM, status) ~  MS+ DOP + Quantile + age +as.factor(color_cat), data=df_A, mincriterion=0.999999)
# #}
# 
# plot(Models[[i]])
# text(Models[[i]])
# 
# 
# plot(Models1[[i]])
# text(Models1[[i]])
# 
# Split in test and training ----------------------------------------------
df_A$rows<- rownames(df_A)
setkey(df_A, "rows")
set.seed(42)
split<- sample(rownames(df_A), size=floor(0.6*nrow(df_A)))


train<- df_A[split,]
valid<- df_A[!split,]

i<- "A_test"
Models_rpart<- NULL
Models_ctree<- NULL
Models_cph<- NULL
Models_KM<- NULL

Models_rpart[[i]]<- rpart(Surv(newTOM, status) ~ MS+ DOP + Quantile + age, data=train)
Models_ctree[[i]]<- ctree(Surv(newTOM, status) ~ MS+ DOP + Quantile + age, data=train, mincriterion=0.99)
Models_cph[[i]]<- coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age, data=train)
Models_KM[[i]]<- survfit(Surv(newTOM, status) ~ 1, type="kaplan-meier", data=train)

times<- seq(from=1, to=30, by=1)
#Predict the mean survival time:
predict_ctree<- predict(Models_ctree[["A_test"]], newdata=valid, type="response")
predict_rpart<- Pred.rpart(Models_rpart[["A_test"]], newdata=valid)
predict_cph<- predictSurvProb(Models_cph[["A_test"]],  newdata=valid[1:20,], times=seq(from=1, to=30, by=1))
predict_KM<- predictSurvProb(Models_KM[["A_test"]],  newdata=valid, times=c(1,30))

#NB!!! Look at predictSurvProb Package pec




# AUC measure -------------------------------------------------------------
# 
# Surv.rsp <- Surv(train$newTOM, train$status)
# Surv.rsp.new <- Surv(valid$newTOM, valid$status)
# times <- seq(1, 30, 1)
# AUC_hc <- AUC.hc(Surv.rsp, Surv.rsp.new, predict_cph, times)
# AUC_hc
# 
# 
# # Brier score and other measures ------------------------------------------
# lp<- predict(Models_cph[["A_test"]], train)
# 
# predErr(Surv.rsp, Surv.rsp.new, lp, predict_cph, times,
#         type = "brier", int.type = "unweighted")
# predErr(Surv.rsp, Surv.rsp.new, lp, lpnew, times,
#         type = "robust", int.type = "unweighted")
# predErr(Surv.rsp, Surv.rsp.new, lp, lpnew, times,
#         type = "brier", int.type = "weighted")
# 


# LTRCTrees ---------------------------------------------------------------
LTRCART.pred <- Pred.rpart(Surv(newTOM, status) ~ MS + DOP + Quantile + age, train, valid)
LTRCART.pred$KMcurves  ## list of predicted KM curves
LTRCART.pred$Medians  ## vector of predicted median survival time


# Median survival times for cox -------------------------------------------

#mediantime_cph <- read.table(textConnection(capture.output(survfit(Models_cph[["A_test"]], newdata= valid))),skip=2,header=TRUE)$median



# Other models ------------------------------------------------------------

library("rms")

library("randomForestSRC")

library("party")


fitform <- Surv(newTOM,status)~ MS + DOP + Quantile + age
fitcox <- selectCox(fitform, data=train, rule="aic")
fitrpart<- pecRpart(fitform, data=train)
set.seed(13)

fitrsf <- rfsrc(fitform,data=train[1:1000,],forest=TRUE,ntree=5)

set.seed(13)

fitcforest <- pecCforest(fitform, data=train[1:1000,], controls=cforest_classical(ntree=3))

pcox <- predictSurvProb(fitcox,newdata=valid,times=20)

prsf <- predictSurvProb(fitrsf,newdata=valid,times=20)

extends <- function(...)TRUE

pcf <- predictSurvProb(fitcforest,newdata=valid,times=20)

prpart <- predictSurvProb(fitrpart,newdata=valid,times=20)

extends <- function(...)TRUE

set.seed(2006)

fitpec <- pec(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=train, cens.model="cox", splitMethod="Boot632plus", maxtime=2000, B=5, keep.index=TRUE, keep.matrix=TRUE)

crps.t20 <- crps(fitpec,times=20)

crps.t2000


fitrpart<- rpart(fitform, data=train)
extends<- function(...)TRUE
predictSurvProb(fitrpart)


# Example -----------------------------------------------------------------


set.seed(18713)
library(prodlim)
library(survival)
dat=SimSurv(100)
pmodel=coxph(Surv(time,status)~X1+X2,data=dat)
perror=pec(list(Cox=pmodel),Hist(time,status)~1,data=dat)

## cumulative prediction error
crps(perror,times=1) # between min time and 1
## same thing:
ibs(perror,times=1) # between min time and 1
crps(perror,times=1,start=0) # between 0 and 1
crps(perror,times=seq(0,1,.2),start=0) # between 0 and seq(0,1,.2)