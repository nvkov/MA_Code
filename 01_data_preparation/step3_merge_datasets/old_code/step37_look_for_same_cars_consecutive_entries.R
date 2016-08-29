#Find vendors with several vendor_IDs

#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<- "/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("Merged_data/df_merge_after_step35.RData")


df_merge<- df_merge[ , .(car_ID=max(car_ID), car_IDs=paste(car_ID, collapse = ","),
                         cars_lastDate=max(cars_lastDate), cars_lastChange=min(cars_lastChange),
                         prices_firstDate=min(prices_firstDate), prices_lastDate=max(prices_lastDate),
                         TOM=sum(TOM), Anzeigenanlage=min(Anzeigenanlage),
                         rows=.N), 
                    by=.(valuePrice, Typ, Kategorie, Farbe, HU, Erstzulassung, 
                         Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                         Hubraum, Eigenschaften, Kilometer, vendor_ID) ]


save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step37.RData" ))

