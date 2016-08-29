rm(list=ls())
library(data.table)
library(dplyr)
#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
#load("hashtable.RData")
load("df_step23.RData")

set<- df[df$Typ=="A180",]


save(set, file=paste0(wd, "betaset.Rdata"))
