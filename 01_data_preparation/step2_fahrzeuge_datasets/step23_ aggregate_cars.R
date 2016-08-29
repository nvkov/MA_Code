#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("cars_full_after_step22.RData")

setwd(project_directory)
sink("cars_descriptive_statistics.txt", append=T)
print("Number of cars before aggregating cars with the same parameters (step23)")
nrow(df)
sink()

setnames(df, "HandlerID", "vendor_ID")

df<- df[, .(cars_lastDate=max(maxDatum)), 
                  by=.(MobileID, Anzeigenanlage, LetzteAenderung, Typ, Kategorie,
                  Eigenschaften, Farbe,  Kilometer,  HU,  Erstzulassung,  Emission,  Kraftstoff,  
                  Leistung, Schaltung,  Klimatisierung, Hubraum,  vendor_ID) ] 

sink("cars_descriptive_statistics.txt", append=T)
print("Number of cars after aggregating cars with the same parameters (step23)")
nrow(df)
sink()

setnames(df, "LetzteAenderung", "cars_lastChange")

#-------------------------------------------------------------------------------
save(df, file=paste0(project_directory, data_directory, "cars_full_after_step23.RData"))