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
df_merge$HU<-as.numeric(gsub("/", "", df_merge$HU))

#Reduce HU:

df_merge<- df_merge[ , .(cars_lastChange=min(cars_lastChange), 
                         cars_lastDate=max(cars_lastDate), HU=max(HU), 
                         newHU= paste(HU, collapse=","),
                         varHU= var(HU, na.rm=T) , rows=.N), 
            by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
                 Anzeigenanlage, Typ, Kategorie,Farbe, Kilometer, 
                 Erstzulassung, Emission, Kraftstoff, 
                 Leistung, Schaltung, Klimatisierung, Hubraum, Eigenschaften, vendor_ID)]

#Look at varHU>0 and newHU to understand the logic of each vendor:
#----------------------------------------------------------
df_merge$newHU<- NULL
df_merge$varHU<- NULL

df_merge<- df_merge[ , .(cars_lastChange=min(cars_lastChange), 
                         cars_lastDate=max(cars_lastDate), Leistung=max(Leistung), 
                         newLe= paste(Leistung, collapse=","),
                         varLe= var(Leistung, na.rm=T) , rows=.N), 
                     by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
                          Anzeigenanlage, Typ, Kategorie,Farbe, Kilometer, 
                          Erstzulassung, Emission, Kraftstoff, 
                          HU, Schaltung, Klimatisierung, Hubraum, Eigenschaften, vendor_ID)]

df_merge$Leistung<- str_sub(df_merge$newLe, start= -3)
df_merge$Leistung<- gsub(",", "", df_merge$Leistung)

#Reasons for mistakes
# ID: 157122810 - Typo 412, 142
# ID: 129670524 - Missed comma: 699, 70
# ID: 87236294 - Hubraum instead of Leistung
# ID: 127370440 - PS and Kw mixed:136,82,102
df_merge$newLe<- NULL
df_merge$varLe<- NULL

df_merge<- df_merge[ , .(cars_lastChange=min(cars_lastChange), 
                         cars_lastDate=max(cars_lastDate), Emission=max(Emission), 
                         newEm= paste(Emission, collapse=","),
                         varEm= var(Emission, na.rm=T) , rows=.N), 
                     by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
                          Anzeigenanlage, Typ, Kategorie,Farbe, Kilometer, 
                          Erstzulassung, Leistung, Kraftstoff, 
                          HU, Schaltung, Klimatisierung, Hubraum, Eigenschaften, vendor_ID)]

df_merge$Emission<- str_sub(df_merge$newEm, start= -2)

df_merge<- df_merge[ , .(cars_lastChange=min(cars_lastChange), 
                         cars_lastDate=max(cars_lastDate), Hubraum=max(Hubraum), 
                         newHubraum= paste(Hubraum, collapse=","),
                         varHubraum= var(Hubraum, na.rm=T) , rows=.N), 
                     by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
                          Anzeigenanlage, Typ, Kategorie,Farbe, Kilometer, 
                          Erstzulassung, Leistung, Kraftstoff, 
                          HU, Schaltung, Klimatisierung, Emission, Eigenschaften, vendor_ID)]

df_merge$varHubraum<- NULL
df_merge$newHubraum<- NULL


save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step32.RData" ))

