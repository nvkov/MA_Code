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
load("Merged_data/df_merge_after_step33.RData")

df_merge<- df_merge[, .(car_ID=max(car_ID), vendor_ID=max(vendor_ID),
                        , cars_lastDate=max(cars_lastDate), cars_lastChange=min(cars_lastChange)), 
                    by=.(valuePrice, TOM, prices_firstDate, prices_lastDate, 
                         Anzeigenanlage, Typ, Kategorie, Farbe, HU, Erstzulassung, 
                         Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                         Hubraum, Eigenschaften, Kilometer)]

