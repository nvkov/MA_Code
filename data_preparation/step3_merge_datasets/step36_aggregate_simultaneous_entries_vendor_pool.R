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
load("Merged_data/df_merge_after_step35.RData")

# Subset for leasing cars -------------------------------------------------


# Aggregate for vendor pools----- -----------------------------------------

df_merge<- df_merge[, `:=`(#length_merged_car=length(unique(car_ID)), 
                          #car_IDs=paste(car_ID, collapse=","),
                          vendor_IDs=paste(vendor_ID, collapse=","),
                          varCar=I(max(car_ID)-min(car_ID)), 
                          varVendor=var(vendor_ID), 
                          rows=.N
                          ), 
                    by=.(valuePrice,  
                         Typ, Kategorie, Farbe, HU, Erstzulassung, 
                         Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                         Hubraum, Eigenschaften, Kilometer, prices_firstDate)]

# # See if merge is correct -------------------------------------------------

######################

save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step36.RData" ))
