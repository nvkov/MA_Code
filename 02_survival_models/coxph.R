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
load("ready_for_survival.RData")

df_A<- df1[grep("E", df1$Class),]
#rm(df1)

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


fitform <- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor



# Fit models --------------------------------------------------------------

#(1) Cox proportional hazards:
fitcox <- selectCox(fitform, data=train, rule="aic")
# 
# #(2) CART
# fitrpart<- pecRpart(fitform, data=train)
# 
# #(3) Random Survival Forest:
# set.seed(13)
# 
# fitrsf <- rfsrc(fitform,data=train,forest=TRUE,ntree=50)
# 
# #(4) Conditional forest:
# set.seed(13)
# 
# fitcforest <- pecCforest(fitform, data=train, controls=cforest_classical(ntree=50))
# 
# #(5) Conditional tree:
# fitctree <- pecCtree(fitform, data=train)
# 
# 
# # Find prediction probabilities -------------------------------------------
# 
# pcox <- predictSurvProb(fitcox,newdata=valid,times=2)
# pcox.old<- predictSurvProb(fitcox,newdata=train,times=2)
# 
# prsf <- predictSurvProb(fitrsf,newdata=valid,times=2)
# prsf.old <- predictSurvProb(fitrsf,newdata=train,times=2)
# 
# extends <- function(...)TRUE

pcf <- predictSurvProb(fitcforest,newdata=valid,times=2)
pcf.old<- predictSurvProb(fitcforest,newdata=train,times=2)
# 
# extends <- function(...)TRUE
# 
# prpart <- predictSurvProb(fitrpart,newdata=valid,times=2)
# prpart.old <- predictSurvProb(fitrpart,newdata=train,times=2)
# 
# pctree<- predictSurvProb(fitctree,newdata=valid,times=2)
# pctree.old<- predictSurvProb(fitctree,newdata=train,times=2)
# 
# prsf<- predictSurvProb(fitrsf,newdata=valid,times=2)
# prsf.old<- predictSurvProb(fitrsf,newdata=train,times=2)
# 
# 
# extends <- function(...)TRUE
# 
# 
# # Find AUCs ---------------------------------------------------------------
# #Cox
# # coxAUC<- AUC.hc(Surv(train$newTOM, train$status), Surv(valid$newTOM, valid$status), pcox, times=c(1:100))$auc
# # rsfAUC<- AUC.hc(Surv(train$newTOM, train$status), Surv(valid$newTOM, valid$status), prsf, times=c(1:100))$auc
# # cfAUC<- AUC.hc(Surv(train$newTOM, train$status), Surv(valid$newTOM, valid$status), pcf, times=c(1:100))$auc
# # rpartAUC<- AUC.hc(Surv(train$newTOM, train$status), Surv(valid$newTOM, valid$status), prpart, times=c(1:100))$auc
# # 
# 

# Define parameters for AUc computation -----------------------------------


Surv.rsp <- Surv(train$newTOM, train$status)
Surv.rsp.new <- Surv(valid$newTOM, valid$status)
times <- c(1:100)




# Save separate errors ----------------------------------------------------
coxBS<- pec(list("Cox"=fitcox), formula=Surv(newTOM,status)~1, data=valid, times=c(1:10))$AppErr$Cox


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




# Select parameters -------------------------------------------------------
cox.Class<- c("A", "B", "C", "E",  "ML", "S", "SL", "SLK")
cox.Vars<- c("MS", "DOP", "Quantile", "age", "size_vendor")
# creating parameter grids
cox.grid <- expand.grid(Class=cox.Class)
grid   <- list(cox.grid) 

# calculating number of models
models <- 1 + nrow(cox.grid) 

# creating model arrays
m.cox <- array(vector(mode = "list", length = 1), c(2, nrow(cox.grid)))

# creating error matrix
errors <- vector(mode = "numeric", length = models)
names(errors) <- rep("RFS", length(errors))

# setting up the clock
time.set <- array(vector(mode = "list", length = 1), c(1, 10))
time.set[[1, 1]] <- time.start

coeffs<- matrix("NA",nrow=length(cox.Class), ncol=5)
# Calculate models --------------------------------------------------------

print("Estimating Cox...")
for (i in 1:nrow(cox.grid)) {
  
  # displaying model number
  print(paste0("Model ", i, " out of ", nrow(cox.grid)))
  
  # training models
  m.cox[[i]] <-coxph(fitform, data=df1[df1$Class==cox.grid$Class[i],])
  coeffs[i,1]<- round(m.cox[i][[1]]$coefficients["MS"],3)
  coeffs[i,2]<- round(m.cox[i][[1]]$coefficients["DOP"],3)
  coeffs[i,3]<- round(m.cox[i][[1]]$coefficients["Quantile"],3)
  coeffs[i,4]<- round(m.cox[i][[1]]$coefficients["age"],3)
  coeffs[i,5]<- round(m.cox[i][[1]]$coefficients["size_vendor"],3)
}  
  

# information
time.set[[1, 4]] <- proc.time()
time.trial <- time.set[[1, 4]] - time.set[[1, 3]]
print(paste0("Random Survival forest took ", round(time.trial[3]/60, digits = 0), " minutes."))
#beep(5)


# Save models in latex table ----------------------------------------------
sink()


# EClass ------------------------------------------------------------------
sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cph_Classes.tex")
stargazer(m.cox[1],m.cox[2],m.cox[3],m.cox[4],m.cox[5],m.cox[6],m.cox[7],m.cox[8],
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphE",
          title="Cox proportional hazards. Comparing buyer preferences for major Mercedes-Benz Classes", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Size vendor"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("A", "B", "C", "E", "M", 
                            "S", "SL", "SLK"),  
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()

save(gridsearch.rfs, file="C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/gridserach.rfs5000.RData")
