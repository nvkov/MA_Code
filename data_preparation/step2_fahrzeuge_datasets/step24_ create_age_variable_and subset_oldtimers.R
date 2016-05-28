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
load("cars_full_after_step23.RData")

#Change format of dates for Erstzulassung and lastChange:
df$Erstzulassung<- paste0(df$Erstzulassung, "/01")
df$Erstzulassung<- as.IDate(df$Erstzulassung, format="%Y/%m/%d")

df$cars_lastDate<- as.IDate(as.character(df$cars_lastDate), format="%Y%m%d")
df$cars_lastChange<- as.IDate(as.character(df$cars_lastChange), format="%Y%m%d")

#Create Age variable

df$age<- difftime(df$cars_lastChange, df$Erstzulassung,  units = "days")

#Oldtimer time limit (7 years):

oldtimerAge<- 7*365

#Subset for oldtimers:
df<- df[df$age<oldtimerAge,]


setwd(project_directory)
sink("cars_descriptive_statistics.txt", append=T)
print("Step 24: Number of cars after deleting Oldtimers")
nrow(df)
sink()

#Subset for new cars:
newcarAge<- 7*30

#Subset for oldtimers:
df<- df[df$age>newcarAge,]

setwd(project_directory)
sink("cars_descriptive_statistics.txt", append=T)
print("Step 24: Number of cars after deleting new cars")
nrow(df)
sink()


#-------------------------------------------------------------------------------
save(df, file=paste0(project_directory, data_directory, "cars_full_after_step24.RData"))

