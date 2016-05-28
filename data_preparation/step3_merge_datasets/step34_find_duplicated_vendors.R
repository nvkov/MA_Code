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

irrelevantCols<- c("km_change", "age_check")

df_merge<- df_merge[, -irrelevantCols, with=F]

df_merge<- df_merge[, .(car_ID=paste(car_ID, collapse=","), vendor_ID=paste(vendor_ID, collapse=","), howMany=.N), 
                    by=.(valuePrice, TOM, prices_firstDate, prices_lastDate, 
                         Anzeigenanlage, Typ, Kategorie, Farbe, HU, Erstzulassung, 
                         Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, 
                         Hubraum, Eigenschaften, Kilometer), cars_lastDate, cars_lastChange]

double_vendors<- df_merge[df_merge$howMany>1,]

#save all doubled vendors and car_IDs:
doubled_car_IDs<- double_vendors$car_ID
doubled_vendor_IDs<- double_vendors$vendor_ID

save(double_vendors, file=paste0(project_directory, data_directory, "Merged_data/double_vendors_after_step34.RData" ))


save(doubled_car_IDs, file=paste0(project_directory, data_directory, "Merged_data/doubled_car_IDs_after_step34.RData" ))
save(doubled_vendor_IDs, file=paste0(project_directory, data_directory, "Merged_data/doubled_vendor_IDs_after_step34.RData" ))

doubled_vendors_vec<- paste(doubled_vendor_IDs, collapse=",")
unique_doubled_vendors_vec<- unlist(strsplit(doubled_vendors_vec, ","))
unique_doubled_vendors_vec<- sort(unique(as.numeric(unique_doubled_vendors_vec)))

save(unique_doubled_vendors_vec, file=paste0(project_directory, data_directory, "Merged_data/unique_doubled_vendor_IDs_vec_after_step34.RData" ))
