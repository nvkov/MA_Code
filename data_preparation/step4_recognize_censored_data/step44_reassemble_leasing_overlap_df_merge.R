rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<- "/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("Merged_data/df_merge_after_step38.RData")
load("Merged_data/leasing_after_step35.Rdata")
load("Merged_data/overlaps_after_step35.Rdata")


# Keep only relevant vars -------------------------------------------------
relCols<- c("car_ID", "vendor_ID",  
            "valuePrice", "TOM", 
            "prices_firstDate", "prices_lastDate", 
            "cars_lastChange", "cars_lastDate",
            "Anzeigenanlage", "Typ", "Farbe", "Kilometer", "HU", "Erstzulassung", 
            "Emission", "Kraftstoff", "Leistung", "Schaltung", "Klimatisierung", 
            "Hubraum", "Eigenschaften", "Kategorie")

df_merge<- vendors[,relCols, with=F]
overlaps<- overlaps[,relCols, with=F]
leasing<- leasing[,relCols, with=F]

df<- rbind(df_merge, overlaps, leasing)

save(df, "dataset_final.RData")
