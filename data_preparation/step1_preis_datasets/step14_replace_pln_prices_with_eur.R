rm(list=ls())
library("data.table")
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/TOM/"
setwd(paste0(project_directory, data_directory))

load("TOMmerge.RData")
load("TOMpln.RData")

delete<-tomdf[tomdf$Bemerkung=="PLN" | tomdf$Bemerkung=="PLN, NEGOTIABLE" | tomdf$Bemerkung=="PLN, Netto" ]
tomdf<- tomdf[!delete,]
tompln$x<-NULL

tomdf<-rbind(tomdf, tompln)

save(tompln, file=paste0(project_directory, data_directory,"TOMpln.RData"))
