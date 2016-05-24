rm(list=ls())
library("data.table")
library("sets")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
#load("hashtable.RData")
load("clean_betaset_merge.RData")

marketSize<- function(i, data){
  temp<- length(unique(
    
    data$MobileID[ data$Typ==data$Typ[i] 
                          & data$Kategorie==data$Kategorie[i] 
                          & abs(data$age-data$age[i])<=90 
                          & abs(data$Kilometer-data$Kilometer[i])<=10000 
                          & data$firstDate<=data$lastDate[i]
                          & data$lastDate>= data$firstDate[i]
                          ]
  )
  )
  
  temp2<- length(unique(
    
    data$MobileID[ data$Typ==data$Typ[i] 
                   & data$Kategorie==data$Kategorie[i] 
                   & abs(data$age-data$age[i])<=90 
                   & abs(data$Kilometer-data$Kilometer[i])<=10000 
                   & data$firstDate<=data$lastDate[i]
                   & data$lastDate>= data$firstDate[i]
                   ]
  )
  )
  
  
  print(i)
  return(cbind(temp, temp2))
}


i=100
data<-daten
marketSize(i, data)

xy.list<- as.list(seq(from=1, to=nrow(data), by=1))

mS<- rep(NA, nrow(data))
mS<-t(sapply(xy.list, FUN=marketSize, data = data))
#mS<- as.numeric(as.character(mS))

df<- cbind(data, mS)
save(df, file=paste0(wd,"msdf.RData"))


##=============================================================
# Survival data:
library(survival)
mini.surv <- survfit(Surv(df$TOM)~ df$mS, conf.type="none")
summary(mini.surv)
plot(mini.surv, xlab="Time", ylab="Survival Probability")


