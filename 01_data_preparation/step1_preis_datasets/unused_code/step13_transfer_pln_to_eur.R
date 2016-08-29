rm(list=ls())
library("data.table")
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/TOM/"
setwd(paste0(project_directory, data_directory))

load("TOMmerge.RData")

tomdf$unique_ID<-as.numeric(row.names(tomdf))
setkey(tomdf, "unique_ID")
save(tomdf, file=paste0(project_directory, data_directory, "TOMmerge.RData"))
tompln<- tomdf[tomdf$Bemerkung=="PLN" | tomdf$Bemerkung=="PLN, NEGOTIABLE" | tomdf$Bemerkung=="PLN, Netto" ]
rm(tomdf)

eurpln<- read.csv("eurpln.txt", sep=";")
eurpln$Date<- as.Date(eurpln$Date)


#-----------------
meanXchange<- function(tompln){
x<-  mean(eurpln$Price[eurpln$Date>=tompln$firstDate & eurpln$Date<=tompln$lastDate], na.rm=T)
return(x)
}

meanXchange(tompln[123,])

#Implement (implementation could be faster with lapply - look later into it):
x<- NULL
for(i in 1:nrow(tompln)){
x[i]<- meanXchange(tompln[i,])
print(i)
  }

# Put zloty into Euro: NB function generates NaN for 1643 obs. 
# Reason: missing data on exchange rates for certain days. 
tompln<- cbind(tompln, x)
tompln$valuePrice<-round(tompln$valuePrice/tompln$x)
save(tompln, file=paste0(project_directory, data_directory,"TOMpln.RData"))

