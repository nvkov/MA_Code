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
rm(df)
df1$newMS<- df1$MS/10

df_A<- df1[grep("A", df1$Class),]
df_A<- df_A[1:10000,]
# Survival models ---------------------------------------------------------
Models<- NULL
Models1<- NULL

fitrpart<- rpart(Surv(newTOM, status) ~ MS + DOP + Quantile + age, 
                 data=df_A)

fitctree<- ctree(Surv(newTOM, status) ~  MS+ DOP + Quantile + age, 
                 data=df_A)

fitcph<- coxph(Surv(newTOM, status) ~  MS+ DOP + Quantile + age, 
               data=df_A)

fitrfs<- rfsrc(Surv(newTOM, status) ~  MS+ DOP + Quantile + age, 
               data=df_A, ntree = 100, importance=T)

fitcforest<- cforest(Surv(newTOM, status) ~  MS+ DOP + Quantile + age, 
      data=df_A)


plot(fitrpart)
text(fitrpart)
  
  
plot(fitctree, type="simple")

plot(fitcforest, type="simple")
plot(fitrfs, verbose=T)
