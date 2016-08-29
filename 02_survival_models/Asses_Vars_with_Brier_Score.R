#Check variable relevance with Bagging Brier score:

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

train<- train[1:1500,]
valid<- valid[1:1000]
rm(df_A)


# Select covariates -------------------------------------------------------

#Step 1
fitform <- Surv(newTOM,status)~ MS + DOP + Quantile + age

#Step 2
fitform11<- Surv(newTOM,status)~ MS + DOP + Quantile
fitform12<- Surv(newTOM,status)~ MS + DOP + age
fitform13<- Surv(newTOM,status)~ MS + age + Quantile
fitform14<- Surv(newTOM,status)~ age + DOP + Quantile


# Asses after step 2 ------------------------------------------------------

fitcforest <- pecCforest(fitform, data=train, controls=cforest_classical(ntree=20))

fitcforest11 <- pecCforest(fitform11, data=train, controls=cforest_classical(ntree=20))
fitcforest12 <- pecCforest(fitform12, data=train, controls=cforest_classical(ntree=20))
fitcforest13 <- pecCforest(fitform13, data=train, controls=cforest_classical(ntree=20))
fitcforest14 <- pecCforest(fitform14, data=train, controls=cforest_classical(ntree=20))

fitpec <- pec(list("fitfull"=fitcforest,"fitNoage"=fitcforest11,"fitNoQuantile"=fitcforest12,"fitNoDOP"=fitcforest13, "fitNoMS"=fitcforest14), formula=Surv(newTOM, status)~MS+DOP+Quantile+age, data=valid, times=c(1:30))
 plot(fitpec, smooth=T, legend.x=35)




# Add bagging procedure ---------------------------------------------------

 library(ipred)
 set.seed(20101210)
 mod1 <- bagging(fitform,data=train,coob=T,nbagg=100)
 print(mod1)
 mod2 <- bagging(fitform11,data=train,coob=T,nbagg=100)
 print(mod2)
 mod3 <- bagging(fitform12,data=train,coob=T,nbagg=100)
 print(mod3)
 mod4 <- bagging(fitform13,data=train,coob=T,nbagg=100)
 print(mod4)
 mod5 <- bagging(fitform14,data=train,coob=T,nbagg=100)
 print(mod5)
 




