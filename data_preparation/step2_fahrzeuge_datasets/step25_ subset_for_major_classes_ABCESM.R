#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("cars_full_after_step24.RData")

#Change format of dates for Erstzulassung and lastChange:
df<- df[grep("A|B|C|E|S|M", df$Typ),]
df<- df[!grep("Sprinter|AMG|CL|Citan|T|ANDERE", df$Typ),]

#Correct unexisting names with Hubraum:
df<-df[grep("[A-Z][0-9]{2}0|[A-Z][0-9]{1}5|[0-9]{2}0[A-Z]", df$Typ),]

setwd(project_directory)
sink("cars_descriptive_statistics.txt", append=T)
print("Step 25: Number of cars after subsetting for major classes")
nrow(df)
sink()

#-------------------------------------------------------------------------------
save(df, file=paste0(project_directory, data_directory, "cars_full_after_step25.RData"))

