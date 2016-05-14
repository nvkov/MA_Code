rm(list=ls())
library("data.table")
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/TOM/"
setwd(paste0(project_directory, data_directory))

load("TOMmerge.RData")

tompln<- tomdf[tomdf$Bemerkung=="PLN" | tomdf$Bemerkung=="PLN, NEGOTIABLE" | tomdf$Bemerkung=="PLN, Netto" ]

rm(tomdf)
eurpln<- read.csv("eurpln.txt", sep=";")
eurpln$Date<- as.Date(eurpln$Date)

meanXchange<- function(eurpln, firstDate, lastDate){
  mean(eurpln$Price[eurpln$Date>firstDate & eurpln$Date<lastDate])
}

meanXchange(eurpln, "2010-11-17", "2011-12-18")
meanXchange(eurpln, tompln$firstDate, tompln$lastDate)
