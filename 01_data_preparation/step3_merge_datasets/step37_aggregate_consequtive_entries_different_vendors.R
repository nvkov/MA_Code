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
library(IRanges)


#Read files with car specifications:
load("Merged_data/df_merge_after_step36.RData")

# Aggregate for vendor pools----- -----------------------------------------

df_merge<- df_merge[, `:=`(length_merged_car=length(unique(car_ID)), 
                          #car_IDs=paste(car_ID, collapse=","),
                          vendor_IDs=paste(unique(vendor_ID), collapse=","),
                          vandor_count=length(unique(vendor_ID)), 
                          varCar=I(max(vendor_ID)-min(vendor_ID)), 
                          var_prices_lastDate=var(prices_lastDate),
                          rows=.N
                          ), 
                    by=.(valuePrice,  
                         Typ, Kategorie, Farbe, HU, Erstzulassung, 
                         Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                         Hubraum, Eigenschaften, Kilometer)]

df_merge$check<- abs(df_merge$prices_firstDate-df_merge$Anzeigenanlage) 
View(df_merge[df_merge$rows>1,])

# Check if this is done systematically ------------------------------------

check<- df_merge$vendor_IDs[df_merge$length_merged_car==296][1]
check<- unlist(strsplit(check, ","))

View(df_merge[df_merge$vendor_ID %in% check,])



# Aggregate for vendor pools: ---------------------------------------------


df_merge<- df_merge[, .(car_ID=max(car_ID),
                           vendor_ID=max(vendor_ID),
                           prices_firstDate=min(prices_firstDate),
                           prices_lastDate=max(prices_lastDate),
                           cars_lastChange= min(cars_lastChange),
                           cars_lastDate=max(cars_lastDate), 
                           Anzeigenanlage=min(Anzeigenanlage), 
                           TOM=max(TOM)), 
                            
                     by=.(valuePrice,  
                          Typ, Kategorie, Farbe, HU, Erstzulassung, 
                          Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                          Hubraum, Eigenschaften, Kilometer)]

df_merge$check<- abs(df_merge$prices_firstDate-df_merge$Anzeigenanlage) 

# # See if merge is correct -------------------------------------------------
######################

save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step37.RData" ))
