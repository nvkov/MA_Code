rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
load("fahrzeugeFull.Rdata")

relevantCols<- c("MobileID", "Kilometer", "Anzeigenanlage", "Typ", "Kategorie", 
                 "Erstzulassung", "Emission", "Kraftstoff", "Leistung", 
                 "Schaltung", "HU", "Hubraum", "HandlerID", 
                 "LetzteAenderung", "maxDatum")

irrelCols<- c("Farbe", "Eigenschaften")

df[,irrelCols]<- NULL

save(df, file=paste0(wd, "df_step23.RData"))