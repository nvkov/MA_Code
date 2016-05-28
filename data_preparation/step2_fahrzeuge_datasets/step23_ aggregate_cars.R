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
load("cars_commercial_vendors_full.RData")

setwd(project_directory)
sink("cars_descriptive_statistics.txt", append=T)
print("Number of cars before aggregating cars with the same parameters (step23)")
nrow(df)
sink()

df<- df[, .(cars_lastDate=max(V1)), 
                  by=.(MobileID, Anzeigenanlage, LetzteAenderung, Typ, Kategorie,
                  Eigenschaften, Farbe,  Kilometer,  HU,  Erstzulassung,  Emission,  Kraftstoff,  
                  Leistung, Schaltung,  Klimatisierung, Hubraum,  vendor_ID) ] 

sink("cars_descriptive_statistics.txt", append=T)
print("Number of cars after aggregating cars with the same parameters (step23)")
nrow(df)
sink()

setnames(df, "LetzteAenderung", "cars_lastChange")

#df<- df[, .(Eigenschaften= paste(Reduce(intersect, strsplit(Eigenschaften, ",") ), collapse=","), LetzteAenderung=max(LetzteAenderung), maxDatum=max(maxDatum)), 
#                                   by=.(MobileID, Kilometer, Anzeigenanlage, Typ, Kategorie, Farbe, Erstzulassung,
#                                        Emission, Kraftstoff, Leistung, Schaltung, HU, Klimatisierung,Hubraum, vendor_ID)]

#-------------------------------------------------------------------------------
save(df, file=paste0(project_directory, data_directory, "cars_full_after_step23.RData"))