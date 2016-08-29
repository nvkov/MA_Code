rm(list=ls())
library(data.table)
library(dplyr)
#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
#load("hashtable.RData")
load("betaset.RData")
load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/TOM/TOMmerge.RData")

set$unique_ID<- as.numeric(row.names(set))
setkeyv(set, "MobileID")
setkeyv(tomdf, "MobileID")

set<-tomdf[set]
set<- set[maxDatum>=firstDate & LetzteAenderung<=lastDate,]


save(set, file="betaset_merge.RData")


