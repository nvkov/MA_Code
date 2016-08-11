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
load("Merged_data/df_merge_after_step34.RData")
load("Merged_data/list_of_leasing_cars_after_step34.RData")

# Subset for leasing cars -------------------------------------------------

leasing<- df_merge[df_merge$car_ID %in% leasing_cars$car_ID,]
df_merge<- df_merge[!df_merge$car_ID %in% leasing_cars$car_ID,]

# Aggregate for vendor pools----- -----------------------------------------

df_merge<- df_merge[, `:=`(length_merged_car=length(unique(car_ID)), 
                          car_IDs=paste(car_ID, collapse=","),
                          varCar=I(max(car_ID)-min(car_ID))
                          ), 
                    
                    by=.(vendor_ID, valuePrice,  
                         Typ, Kategorie, Farbe, HU, Erstzulassung, 
                         Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                         Hubraum, Eigenschaften, Kilometer)]

# See if merge is correct -------------------------------------------------
# Check if observtaions from one group overlap:

df_merge<- df_merge[varCar>0, 
                    overlaps:= countOverlaps(IRanges(as.numeric(prices_firstDate), as.numeric(prices_lastDate)), 
                                             IRanges(as.numeric(prices_firstDate), as.numeric(prices_lastDate))),
                    
                    by=.(vendor_ID, valuePrice,  
                         Typ, Kategorie, Farbe, HU, Erstzulassung, 
                         Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                         Hubraum, Eigenschaften, Kilometer)]

overlaps<- df_merge[df_merge$overlaps>2,]
df_merge<- df_merge[df_merge$overlaps<3 | is.na(df_merge$overlaps),]


# Aggregate grouped observations ------------------------------------------

df_merge<- df_merge[, .(car_ID=max(car_ID), 
                         prices_firstDate=min(prices_firstDate), 
                         prices_lastDate=max(prices_lastDate),
                         TOM= sum(TOM), 
                         cars_lastChange=min(cars_lastChange),
                         cars_lastDate=max(cars_lastDate),
                         Anzeigenanlage=min(Anzeigenanlage),
                         rows=.N),
                     
                     by=.(vendor_ID, valuePrice,  
                          Typ, Kategorie, Farbe, HU, Erstzulassung, 
                          Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                          Hubraum, Eigenschaften, Kilometer)]



######################



save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step35.RData" ))
save(leasing, file=paste0(project_directory, data_directory, "Merged_data/leasing_after_step35.RData" ))
save(overlaps, file=paste0(project_directory, data_directory, "Merged_data/overlaps_after_step35.RData" ))
