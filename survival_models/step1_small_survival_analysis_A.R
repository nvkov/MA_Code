#Survival analysis:

rm(list=ls())
library("data.table")
library("sets")
library("survival")



#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/step5/"

load(paste0(project_directory, data_directory, "new_df_merge_C200.RData"))

#put this in a data prep step:
temp$Leistung<- as.numeric(temp$Leistung)



cox_A<- coxph(Surv(newTOM,right_censored) ~ valuePrice + age + age*valuePrice + DOP + MS+
                Erstzulassung + Kraftstoff + Leistung + Schaltung, data=temp)

summary(cox_A)