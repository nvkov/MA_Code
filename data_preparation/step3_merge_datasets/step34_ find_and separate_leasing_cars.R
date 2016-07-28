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

# Find leasing IDs --------------------------------------------------------

df_merge<- df_merge[,#`:=`(car_ID=max(car_ID), 
                      `:=`(leasing_car_IDs=paste(car_ID, collapse = ", "),
                       leasing_count=length(unique(car_ID)), 
                       rangeLeasing= I(max(car_ID)-min(car_ID))),
                    
                    by=.(vendor_ID, valuePrice, prices_firstDate,  
                         Anzeigenanlage, Typ, Kategorie, Farbe, HU, 
                         Erstzulassung, Emission, Kraftstoff, 
                         Leistung, Schaltung, Klimatisierung, Hubraum,  Eigenschaften)]


# Inspect leasing IDs -----------------------------------------------------


View(df_merge[df_merge$leasing_count>1,])

leasing_cars<- df_merge[leasing_count>1 ,c("car_ID", "leasing_count", "rangeLeasing"),with=F]

save(leasing_cars,
     file=paste0(project_directory, 
                 data_directory,
                 file="Merged_data/list_of_leasing_cars_after_step34.RData"))

#save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step34.RData" ))

