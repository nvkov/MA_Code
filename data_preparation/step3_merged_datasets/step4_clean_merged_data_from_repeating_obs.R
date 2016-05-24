rm(list=ls())
library("data.table")
library("sets")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
#load("hashtable.RData")
load("betaset_merge.RData")

daten<- set

rm(set)
#=======================================================================

#Clean up cars with two IDs but the same characteristics

# Find observations with perfect repetition on 
#"Typ", "Kategorie", "Eigenschaften", "Kilometer", "Erstzulassung",
# "Emission", "Kraftstoff", "Schaltung", "HandlerID", "valuePrice"
relevantCols<- c( "valuePrice", "Kilometer", "Typ", 
                 "Kategorie", "Erstzulassung", "Emission", "Kraftstoff", "Leistung", 
                 "Schaltung", "HU", "Klimatisierung", "Hubraum", "HandlerID" )


dups<- duplicated(daten[ , relevantCols, with=F])
subsub<- daten[dups & !duplicated(daten$MobileID), ]

# Extract all the cars with the identified IDs:
dupID<- unique(subsub$MobileID)
sameCars_diffID<- daten[daten$MobileID %in% dupID, ]

# Merge all data rows to extract leasing traders:
exp<- sameCars_diffID[,.(MobileID=max(MobileID), TOM=max(TOM), 
                         Anzeigenanlage=min(Anzeigenanlage), lastDate=max(lastDate),
                         erasedIDs=paste(as.character(MobileID), collapse=","),
                         leasingCount=length(unique(MobileID))), 
                       
                      by=.( Typ, Kategorie,Kilometer, Erstzulassung, Emission, Kraftstoff, 
                             Leistung, Schaltung, Klimatisierung, Hubraum, HandlerID, 
                             LetzteAenderung, valuePrice, firstDate)]

# Extract vector with leasing cars:

leasingDF<- exp[exp$leasingCount>2,]
leasingID<- leasingDF$erasedIDs

                                  
                          
#Merge car info uploaded twice the same day by mistake:

small_pure1<- exp[ ,.(Anzeigeanlage=min(Anzeigenanlage), 
                      lastDate= max(as.IDate(lastDate, format="%Y-%m-%d")), 
                      firstDate=min(as.IDate(firstDate, format="%Y-%m-%d")), 
                      TOM=sum(TOM), 
                      MobileID=max(MobileID), 
                      LetzteAenderung=max(as.IDate(as.character(LetzteAenderung), format="%Y%m%d")), 
                      erasedIDs2=paste(as.character(MobileID), collapse=","),
                      consecCount=length(unique(MobileID))), 
                        
                   by=.(Typ, Kilometer, Kategorie, Erstzulassung,leasingCount, 
                        Emission, Kraftstoff, Leistung, Schaltung, 
                        Klimatisierung, Hubraum, HandlerID, valuePrice)]


#==================================
#Explore 
#eID<- small_pure1$erasedIDs2[small_pure1$consecCount==215]
#eID<- eID[1]
#consecIDs<- unlist(strsplit(eID, ","))
#rep<-exp[as.character(exp$MobileID) %in% consecIDs]


# Market size variable:
small_pure1<- data.table(small_pure1)
small_pure1$Erstzulassung<-paste0(as.character(small_pure1$Erstzulassung), "/01") 


small_pure1$Erstzulassung<- as.IDate(as.character(small_pure1$Erstzulassung), format="%Y/%m/%d")
small_pure1$Anzeigeanlage<- as.IDate(as.character(small_pure1$Anzeigeanlage), format="%Y%m%d")
small_pure1$age<- difftime(small_pure1$lastDate, small_pure1$Erstzulassung, units="days")
#small_pure1$interval<- integers(as.numeric(small_pure1$LetzteAenderung), as.numeric(small_pure1$lastDate))

#Insert extra variable for MarketSize:
#Start with an individual example:

#i=112
#small_pure1$marketSize[i]<- 
#  length(unique(
  
#                  small_pure1$MobileID[ small_pure1$Typ==small_pure1$Typ[i] 
#                                        & small_pure1$Kategorie==small_pure1$Kategorie[i] 
#                                        & abs(small_pure1$age-small_pure1$age[i])<=60 
#                                        & abs(small_pure1$Kilometer-small_pure1$Kilometer[i])<=10000 
#                                        & small_pure1$firstDate<=small_pure1$lastDate[i]
#                                        & small_pure1$lastDate>= small_pure1$firstDate[i]
#                                       ]
#                    )
#                  )


#===============================================
#temp<- NULL
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
  print(i)
  return(temp)
}

data<- small_pure1
rm(small_pure1)

i=100
marketSize(i, data)

xy.list<- as.list(seq(from=1, to=nrow(data), by=1))

mS<- rep(NA, nrow(data))
mS<-sapply(xy.list, FUN=marketSize, data = data)
mS<- as.numeric(as.character(mS))

df<- cbind(data, mS)
save(df, file=paste0(wd,"msdf.RData"))


##=============================================================
# Survival data:
library(survival)
mini.surv <- survfit(Surv(df$TOM)~ df$mS, conf.type="none")
summary(mini.surv)
plot(mini.surv, xlab="Time", ylab="Survival Probability")


