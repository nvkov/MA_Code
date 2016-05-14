rm(list=ls())
library("data.table")
#library("sets")
project_directory<- getwd()
data_directory<- "/generatedData/TOM/"
setwd(paste0(project_directory, data_directory))

filenames<- list.files(pattern="TOMMobilePreise*")

tomdf<- do.call(`rbind`,lapply(filenames, fread, sep=";"))
tomdf<- tomdf[,.(TOM=sum(TOM)), by=.(MobileID, valuePrice, Bemerkung)]

save(tomdf, file=paste0(project_directory, data_directory, "TOMmerge.RData"))
#
