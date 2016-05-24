#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())


project_directory<- "C:/Users/Nk/Documents/Uni/MA/" 
data_directory<- "Pkw/MobileDaten/"   
#Set working directory
setwd(project_directory)


library("data.table")

df<- fread(paste0(project_directory, data_directory, "MobileHaendler20121218.txt"), sep=";")
#Write a variable for the distance between Erstzulassung and HU:

sink("vendor_descriptive_stats.txt")
print("Vendor number before subsetting for German vendors")
nrow(df)
sink()

df<- subset(df, df$Land=="DE")

sink("vendor_descriptive_stats.txt", append=T)
print("Vendor number after subsetting for German vendors")
nrow(df)
sink()

df<- subset(df, df$Kommerziell=="Ja")

sink("vendor_descriptive_stats.txt", append=T)
print("Vendor number after subsetting for commercial vendors")
nrow(df)
sink()

setnames(df, "ID", "vedor_ID")
