rm(list=ls())
library("data.table")
#library("sets")

setwd("H:\\MA\\Pkw\\generatedData\\neg\\")

filenames<- list.files(pattern="negotiableMobilePreise*")[1:3]

#df<- fread("negmerge.txt", sep=";")

negdf<- do.call(`rbind`,lapply(filenames, fread, sep=";"))
negdf$Date<- as.IDate(negdf$Date, format="%Y-%m-%d")
negdf$Date<- as.IDate(negdf$Date, format="%Y-%m-%d")
negdf<- negdf[,.(Date=max(Date), Date=min(Date), TOM=sum(TOM)), by=.(MobileID, valuePrice)]
#negdf<- negdf[,.(neg=sum(neg)), by=.(as.numeric(MobileID), as.numeric(valuePrice))]

write.table(negdf, "H:\\MA\\Pkw\\generatedData\\neg\\Bemerkungenmerge160422.txt", sep=";", row.names=F)

#negdf<- negdf[cardf, allow.cartesian=TRUE]