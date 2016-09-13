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



rm(df)
df1$newMS<- df1$MS/10
# Survival models ---------------------------------------------------------
df_A<- df1[df1$Class=="A"]

rm(df1)

# Split in test and training ----------------------------------------------
df_A$rows<- rownames(df_A)
setkey(df_A, "rows")
set.seed(42)
split<- sample(rownames(df_A), size=floor(0.6*nrow(df_A)))


train<- df_A[split,]
valid<- df_A[!split,]

train<- train[30000:35000,]
valid<- valid[3000:4500]
rm(df_A)

fitform <- Surv(newTOM,status)~ MS + DOP + Quantile + age
fitcox <- selectCox(fitform, data=train, rule="aic")

fitrpart<- pecRpart(fitform, data=train)
set.seed(13)

fitrsf <- rfsrc(fitform,data=train,forest=TRUE,ntree=100, mtry=2)

set.seed(13)

fitcforest <- pecCforest(fitform, data=train, controls=cforest_classical(ntree=100, mtry=2))

fitctree<- pecCtree(fitform, data=train)

# pcox <- predictSurvProb(fitcox,newdata=valid,times=20)
# 
# prsf <- predictSurvProb(fitrsf,newdata=valid,times=20)
# 
# extends <- function(...)TRUE
# 
# pcf <- predictSurvProb(fitcforest,newdata=valid,times=20)
# 
# extends <- function(...)TRUE
# 
# prpart <- predictSurvProb(fitrpart,newdata=valid,times=20)

extends <- function(...)TRUE

set.seed(2006)

#Integrated Brier Score:
fitpec <- pec(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart, "ctree"=fitctree), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid[1:1000])

plot(fitpec, smooth=T, legend.x=130)

#Concordance index:
C.Index <- cindex(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid[1:1000], eval.times=seq(1,250,1))
plot(C.Index)


# crps.t20 <- crps(fitpec,times=c(2:200))
# 
# fitpec2 <- pec(list("Cox"=fitrpart), formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, cens.model="cox", splitMethod="Boot632plus", maxtime=2000, B=5, keep.index=TRUE, keep.matrix=TRUE)
# 
# crps.t20
# 
# par(mfrow=c(2,2))
# plot(crps.t20$BootCvErr[1,], type="l", col="black")
# lines(crps.t20$BootCvErr[2,], col="red")
# lines(crps.t20$BootCvErr[3,], col="green")
# lines(crps.t20$BootCvErr[4,], col="blue")
# 
# plot(crps.t20$AppErr[1,], type="l", col="black", ylim=c(0,0.5))
# lines(crps.t20$AppErr[2,], col="red")
# lines(crps.t20$AppErr[3,], col="green")
# lines(crps.t20$AppErr[4,], col="blue")
# 
# plot(crps.t20$NoInfErr[1,], type="l", col="black", ylim=c(0,0.5))
# lines(crps.t20$NoInfErr[2,], col="red")
# lines(crps.t20$NoInfErr[3,], col="green")
# lines(crps.t20$NoInfErr[4,], col="blue")
# 
# plot(crps.t20$Boot632plusErr[1,], type="l", col="black", ylim=c(0,0.5))
# lines(crps.t20$Boot632plusErr[2,], col="red")
# lines(crps.t20$Boot632plusErr[3,], col="green")
# lines(crps.t20$Boot632plusErr[4,], col="blue")
# 
# 
# fitrpat(pecRpart())

