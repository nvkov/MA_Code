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
load("Merged_data/df_merge_after_step31.RData")

#Build the intersect of Eigenschaften:

df_merge<- df_merge[ , .(Eigenschaften= paste(Reduce(intersect, strsplit(Eigenschaften, ",") ), collapse=","), cars_lastChange=min(cars_lastChange), cars_lastDate=max(cars_lastDate),
                        age_check=max(age)-min(age), age=max(age), HUnew=paste(HU, collapse = ",")), 
            by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
                 Anzeigenanlage, Typ, Kategorie,Farbe, Kilometer, HU, 
                 Erstzulassung, Emission, Kraftstoff, 
                 Leistung, Schaltung, Klimatisierung, Hubraum, vendor_ID)]
#----------------------------------------------------------

#Aggregate on Kilometers:
df_merge<- df_merge[,.(Kilometer=max(Kilometer), km_change=max(Kilometer)-min(Kilometer),
                       age_check=sum(age_check),cars_lastChange=min(cars_lastChange), cars_lastDate=max(cars_lastDate)),
                    by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
                         Anzeigenanlage, Typ, Kategorie,Farbe, HU, 
                         Erstzulassung, Emission, Kraftstoff, 
                         Leistung, Schaltung, Klimatisierung, Hubraum, vendor_ID, Eigenschaften)]



save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step32.RData" ))

