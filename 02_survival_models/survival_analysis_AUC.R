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
library("rms")
library("randomForestSRC")
library("party")
library("ipred")
library("survivalROC")

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
            "Leistung", "Typ","status", "vendor_ID", "car_ID", "prices_firstDate", "prices_lastDate", "Kategorie", "right_censor") 
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


rm(df)
df1$newMS<- df1$MS/10

df_A<- df1[grep("E", df1$Class),]
rm(df1)

# Split in test and training ----------------------------------------------
df_A$rows<- rownames(df_A)
setkey(df_A, "rows")
set.seed(42)
split<- sample(rownames(df_A), size=floor(0.6*nrow(df_A)))


train<- df_A[split,]
valid<- df_A[!split,]

train<- train[1:150000,]
valid<- valid[1:1000]
rm(df_A)


# Select covariates -------------------------------------------------------


fitform <- Surv(newTOM,status)~ MS + DOP + Quantile + age



# Fit models --------------------------------------------------------------

#(1) Cox proportional hazards:
fitcox <- selectCox(fitform, data=train, rule="aic")

#(2) CART
fitrpart<- pecRpart(fitform, data=train)

#(3) Random Survival Forest:
set.seed(13)

fitrsf <- rfsrc(fitform,data=train,forest=TRUE,ntree=50)

#(4) Conditional forest:
set.seed(13)

fitcforest <- pecCforest(fitform, data=train, controls=cforest_classical(ntree=50))

#(5) Conditional tree:
fitctree <- pecCtree(fitform, data=train)


# Find prediction probabilities -------------------------------------------

pcox <- predictSurvProb(fitcox,newdata=valid,times=2)
pcox.old<- predictSurvProb(fitcox,newdata=train,times=2)

prsf <- predictSurvProb(fitrsf,newdata=valid,times=2)
prsf.old <- predictSurvProb(fitrsf,newdata=train,times=2)

extends <- function(...)TRUE

pcf <- predictSurvProb(fitcforest,newdata=valid,times=2)
pcf.old<- predictSurvProb(fitcforest,newdata=train,times=2)

extends <- function(...)TRUE

prpart <- predictSurvProb(fitrpart,newdata=valid,times=2)
prpart.old <- predictSurvProb(fitrpart,newdata=train,times=2)

pctree<- predictSurvProb(fitctree,newdata=valid,times=2)
pctree.old<- predictSurvProb(fitctree,newdata=train,times=2)

prsf<- predictSurvProb(fitrsf,newdata=valid,times=2)
prsf.old<- predictSurvProb(fitrsf,newdata=train,times=2)


extends <- function(...)TRUE


# Find AUCs ---------------------------------------------------------------
#Cox
# coxAUC<- AUC.hc(Surv(train$newTOM, train$status), Surv(valid$newTOM, valid$status), pcox, times=c(1:100))$auc
# rsfAUC<- AUC.hc(Surv(train$newTOM, train$status), Surv(valid$newTOM, valid$status), prsf, times=c(1:100))$auc
# cfAUC<- AUC.hc(Surv(train$newTOM, train$status), Surv(valid$newTOM, valid$status), pcf, times=c(1:100))$auc
# rpartAUC<- AUC.hc(Surv(train$newTOM, train$status), Surv(valid$newTOM, valid$status), prpart, times=c(1:100))$auc
# 


# Define parameters for AUc computation -----------------------------------


Surv.rsp <- Surv(train$newTOM, train$status)
Surv.rsp.new <- Surv(valid$newTOM, valid$status)
times <- c(1:100)


# # CoxPH -------------------------------------------------------------------
# 
# 
# cox.fit <- coxph(Surv(newTOM, status) ~ MS + DOP + Quantile + age,
#                  x=TRUE, y=TRUE, method="breslow", data=train)
# lp.cox <- predict(train.fit)
# lpnew.cox <- predict(train.fit, newdata=valid)
# 
# # Rpart -------------------------------------------------------------------
# 
# 
# rpart.fit <- rpart(Surv(newTOM, status) ~ MS + DOP + Quantile + age,
#                    data=train)
# partyrpart<- as.party(fpart.fit)
# plot(partyrpart)
# 
# lp.rpart <- predict(rpart.fit)
# lpnew.rpart <- predict(rpart.fit, newdata=valid)
# 
# 
# # Ctree -------------------------------------------------------------------
# 
# 
# ctree.fit <- ctree(Surv(newTOM, status) ~ MS + DOP + Quantile + age,
#                    data=train)
# lp.ctree <- predict(ctree.fit, type="prob")
# lpnew.ctree <- predict(ctree.fit, newdata=valid,type="prob")
# 
# 
# # Cforest -----------------------------------------------------------------
# 
# 
# cforest.fit <- cforest(Surv(newTOM, status) ~ MS + DOP + Quantile + age,
#                    data=train)
# lp.cforest <- predict(cforest.fit)
# lpnew.cforest <- predict(cforest.fit, newdata=valid)
# 
# 
# # RSF ---------------------------------------------------------------------
# 
# 
# rsf.fit <- rfsrc(Surv(newTOM, status) ~ MS + DOP + Quantile + age,
#                        data=train)
# lp.rsf <- predict(rsf.fit, train, OOB=T)$survival[,1]
# lpnew.rsf <- predict(rsf.fit, valid, OOB=T)$survival[,1]
# 
# 
# AUC_sh.cox <- AUC.sh(Surv.rsp, Surv.rsp.new, lp.cox, lpnew.cox, times)
# plot(AUC_sh.cox)
# abline(h = 0.5)

# Print AUC ---------------------------------------------------------------


AUC_sh.cox <- AUC.sh(Surv.rsp, Surv.rsp.new, pcox.old, pcox, times)$auc
AUC_sh.rpart <- AUC.sh(Surv.rsp, Surv.rsp.new, prpart.old, prpart, times)$auc
AUC_sh.ctree <- AUC.sh(Surv.rsp, Surv.rsp.new, pctree.old, pctree, times)$auc
AUC_sh.cforest <- AUC.sh(Surv.rsp, Surv.rsp.new, pcf.old, pcf, times)$auc
AUC_sh.rsf <- AUC.sh(Surv.rsp, Surv.rsp.new, prsf.old, prsf, times)$auc



# Plot AUC for different times --------------------------------------------


plot(AUC_sh.cox, col="red", type="l", ylim=c(0.4,0.6), xlim=c(0,20))
abline(h = 0.5)
lines(AUC_sh.rpart, col="green")
lines(AUC_sh.ctree, col="blue")
lines(AUC_sh.cforest, col="pink")
lines(AUC_sh.rsf, col="grey")


# 
# # Brier score -------------------------------------------------------------
# IBS.cox <- predErr(Surv.rsp, Surv.rsp.new, pcox.old, pcox, times)$error
# IBS.rpart <- predErr(Surv.rsp, Surv.rsp.new, prpart.old, prpart, times)$error
# IBS.ctree <- predErr(Surv.rsp, Surv.rsp.new, pctree.old, pctree, times)$error
# IBS.cforest <- predErr(Surv.rsp, Surv.rsp.new, pcf.old, pcf, times)$error
# IBS.rsf <- predErr(Surv.rsp, Surv.rsp.new, prsf.old, prsf, times)$error

# 
# # Plot Brier score --------------------------------------------------------
# plot(IBS.cox, col="red", type="l")
# lines(IBS.rpart, col="green")
# lines(IBS.ctree, col="blue")
# lines(IBS.cforest, col="pink")
# lines(IBS.rsf, col="grey")

# Plot Brier score from pec -----------------------------------------------

#Integrated Brier Score:
#fitpec <- pec(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart), formula=Surv(newTOM,status)~1, data=valid, times=c(1:10))
# plot(fitpec, smooth=T, legend.x=35)


# Save separate errors ----------------------------------------------------
coxBS<- pec(list("Cox"=fitcox), formula=Surv(newTOM,status)~1, data=valid, times=c(1:10))$AppErr$Cox
rsfBS<- pec(list("rsf"=fitrsf), formula=Surv(newTOM,status)~1, data=valid, times=c(1:10))$AppErr$rsf
rpartBS<- pec(list("rpart"=fitrpart), formula=Surv(newTOM,status)~1, data=valid, times=c(1:10))$AppErr$rpart
ctreeBS<- pec(list("ctree"=fitctree), formula=Surv(newTOM,status)~1, data=valid, times=c(1:10))$AppErr$ctree
cforestBS<- pec(list("cforest"=fitcforest), formula=Surv(newTOM,status)~1, data=valid, times=c(1:10))$AppErr$cforest
referenceBS<- pec(list("Cox"=fitcox), formula=Surv(newTOM,status)~1, data=valid, times=c(1:10))$AppErr$Reference


# Plot the Brier score ----------------------------------------------------

plot(coxBS, type="l", col="red")
lines(rsfBS, col="grey")
lines(rpartBS, col="green")
lines(ctreeBS, col="blue")
lines(cforestBS, col="pink")
lines(referenceBS, col="black")





# Concordance index -------------------------------------------------------

# C.Index <- cindex(list("Cox"=fitcox,"rsf"=fitrsf, "rpart"=fitrpart), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, 
#                   eval.times=c(1:10), pred.times = c(1:10) )
# 
# plot(C.Index)

coxC.Index<- cindex(list("Cox"=fitcox), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, 
                                     eval.times=c(1:60), pred.times = c(1:60))$AppCindex$Cox
rpartC.Index<- cindex(list("rpart"=fitrpart), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, 
                    eval.times=c(1:60), pred.times = c(1:60))$AppCindex$rpart

rsfC.Index<- cindex(list("rsf"=fitrsf), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, 
                    eval.times=c(1:60), pred.times = c(1:60))$AppCindex$rsf

ctreeC.Index<- cindex(list("ctree"=fitctree), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, 
                    eval.times=c(1:60), pred.times = c(1:60))$AppCindex$ctree

cforestC.Index<- cindex(list("cforest"=fitcforest), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, 
                    eval.times=c(1:60), pred.times = c(1:60))$AppCindex$cforest

plot(coxC.Intex, type="l", col="red")
lines(rsfC.Intex, col="grey")
lines(rpartC.Intex, col="green")
lines(ctreeC.Intex, col="blue")
lines(cforestC.Intex, col="pink")
lines(referenceC.Intex, col="black")
