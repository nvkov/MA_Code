#
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory)

setwd(wd)

library("data.table")
library("stringr")

#Load car_data and look for doubles:
load(paste0(project_directory, data_directory, "cars_commercial_vendors_full.RData"))

#Check for problematic IDs:
problematic_car_IDs<- df[, .(length(unique(Typ))), by=.(MobileID, vendor_ID) ]
problematic_car_IDs<- subset(problematic_car_IDs, problematic_car_IDs$V1>1)

sink("cars_descriptive_statistics.txt", append=T)
print("Number of car_IDs that have several cars saved under the same car_ID")
nrow(problematic_car_IDs)

print("Number of vendors that save different cars under the same car_ID")
length(unique(problematic_car_IDs$vendor_ID))

sink()

save(problematic_car_IDs, file=paste0(project_directory, data_directory, "problematic_car_IDs.RData"))