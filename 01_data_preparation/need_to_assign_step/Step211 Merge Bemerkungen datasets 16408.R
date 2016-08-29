rm(list=ls())
library("data.table")
#library("sets")

setwd("H:\\MA\\Pkw\\generatedData\\TOM\\")

filenames<- list.files(pattern="TOMMobilePreise*")[1:12]

#df<- fread("TOMmerge.txt", sep=";")

tomdf<- do.call(`rbind`,lapply(filenames, fread, sep=";"))
tomdf$lastDate<- as.IDate(tomdf$lastDate, format="%Y-%m-%d")
tomdf$firstDate<- as.IDate(tomdf$firstDate, format="%Y-%m-%d")
tomdf<- tomdf[,.(lastDate=max(lastDate), firstDate=min(firstDate), TOM=sum(TOM)), by=.(MobileID, valuePrice)]
#tomdf<- tomdf[,.(TOM=sum(TOM)), by=.(as.numeric(MobileID), as.numeric(valuePrice))]

write.table(tomdf, "H:\\MA\\Pkw\\generatedData\\TOM\\TOMmerge160422.txt", sep=";", row.names=F)

#tomdf<- tomdf[cardf, allow.cartesian=TRUE]