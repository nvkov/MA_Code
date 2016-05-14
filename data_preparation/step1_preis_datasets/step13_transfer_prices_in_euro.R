rm(list=ls())
library("data.table")
project_directory<- getwd()
data_directory<- "/generatedData/TOM/"
setwd(paste0(project_directory, data_directory))

load("TOMmerge.RData")

filenames<- list.files(pattern="TOMMobilePreise*")

tomdf<- do.call(`rbind`,lapply(filenames, fread, sep=";"))
tomdf<- tomdf[,.(TOM=sum(TOM)), by=.(MobileID, valuePrice, Bemerkung)]

save(tomdf, file=paste0(project_directory, data_directory, "TOMmerge.RData"))
#
