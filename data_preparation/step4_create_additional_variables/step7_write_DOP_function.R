rm(list=ls())
library("data.table")
library("sets")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
load("msdf.RData")

DOP_function<- function(df){
modelDOP<- lm(log(df$valuePrice)~df$Erstzulassung + df$Kategorie + df$Kraftstoff) #add df$Typ for full data
df$hedonicPrice<- predict(modelDOP, df)
DOP<- df$valuePrice/exp(df$hedonicPrice)
return(DOP)
}

DOP<-DOP_function(df)


# Survival data:
library(survival)
mini.surv <- survfit(Surv(df$TOM)~ DOP, conf.type="none")
summary(mini.surv)
plot(mini.surv, xlab="Time", ylab="Survival Probability")
