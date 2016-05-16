rm(list=ls())
library(data.table)
library(dplyr)
#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
load("hashtable.RData")
load("df_step23.RData")

#=======================================================================

df$Erstzulassung<-paste0(as.character(df$Erstzulassung), "/01") 
df$Erstzulassung<- as.IDate(as.character(df$Erstzulassung), format="%Y/%m/%d")

#---------------------------------------------------------Begin function
imputeHubraum<- function(type){

dftype<- df[df$Typ==type]
dftype$temp.unique_ID<- as.numeric(row.names(dftype))
keycols=c("Typ", "Leistung", "Kategorie", "Kraftstoff", "Emission", "Schaltung")
setkeyv(dftype, keycols)

keycols=c("Typ", "Leistung", "Kategorie", "Kraftstoff", "Emission", "Schaltung")
setkeyv(specs, keycols)

#specstype<- specs[specs$Typ==type,]

df.mini<- dftype[is.na(dftype$Hubraum), ]

impute<- specs[df.mini, allow.cartesian=T]
impute<-impute[!is.na(HubraumRound),]
impute$Hubraum<-impute$HubraumRound
impute$HubraumRound<- NULL
impute$i.Erstzulassung<-NULL

dftype<- dftype[!dftype$temp.unique_ID %in% impute$temp.unique_ID,]
colorder<- c("MobileID", "Kilometer", "Anzeigenanlage", "Typ", "Kategorie", 
                        "Erstzulassung", "Emission", "Kraftstoff", "Leistung", "Schaltung", 
                        "HU", "Klimatisierung", "Hubraum", "HandlerID", "LetzteAenderung", 
                        "maxDatum", "temp.unique_ID")
setcolorder(dftype, colorder)
setcolorder(impute, colorder)

dftype<- rbind(dftype, impute) 

save(dftype, file=paste0(wd, "ImputedData/", type, ".RData"))
return("Done!")
}

hi<-imputeHubraum("A180")
















