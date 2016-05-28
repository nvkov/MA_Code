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
load("Merged_data/df_merge_after_step32.RData")
load("CleanFahrzeuge/problematic_car_IDs.RData")

df_merge_bad<- subset(df_merge, df_merge$cars_lastChange<=df_merge$prices_firstDate)

df_problematic<- df_merge[df_merge$vendor_ID %in% unique(problematic_car_IDs$vendor_ID),]
df_complement<- df_merge[!df_merge$vendor_ID %in% unique(problematic_car_IDs$vendor_ID),]



#save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step34.RData" ))

