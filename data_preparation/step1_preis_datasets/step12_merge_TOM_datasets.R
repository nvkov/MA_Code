rm(list=ls())
library("data.table")
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/TOM/"
setwd(paste0(project_directory, data_directory))

load("TOMmerge.RData")

