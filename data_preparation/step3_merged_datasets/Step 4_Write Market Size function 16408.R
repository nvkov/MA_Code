rm(list=ls())
library("data.table")
library("sets")
#Set working directory
#setwd("F:\\Pkw\\MobileDaten\\")
#setwd("C:\\Users\\kovachkn.hub\\Desktop\\MA\\Pkw\\MobileDaten\\")
setwd("H:\\MA\\Pkw\\generatedData\\")

daten<- read.table('merge2.txt', sep=' ')

#Clean up cars with two IDs but the same characteristics

# Find observations with perfect repetition on 
#"Typ", "Kategorie", "Eigenschaften", "Kilometer", "Erstzulassung",
# "Emission", "Kraftstoff", "Schaltung", "HandlerID", "valuePrice"
dups<- duplicated(daten[ , c(3, 4,5, 7, 8, 9, 10, 12, 15, 17)])

# Find observations with repetition on 
#"Typ", "Kategorie", "Eigenschaften", "Kilometer", "Erstzulassung", "firstDate",
# "Emission", "Kraftstoff", "Schaltung", "HandlerID", "valuePrice"
#dups1<- duplicated(daten[ , c(3, 4,5, 7, 8, 9, 10, 12, 15, 17, 18)])

# Extract the duplicted data from the table with all repeated observations, but different IDs:
subsub<- daten[dups & !duplicated(daten$MobileID), ]

#subsub1<- daten[dups1 & !duplicated(daten$MobileID), ]

# Extract all the cars with the identified IDs:
dupID<- unique(subsub$MobileID)
sameCars_diffID<- daten[daten$MobileID %in% dupID, ]
sameCars_diffID<- data.table(sameCars_diffID)

#dupID1<- unique(subsub1$MobileID)
#sameCars_diffID1<- daten[daten$MobileID %in% dupID1, ]
#sameCars_diffID1<- sameCars_diffID1[order(sameCars_diffID1$valuePrice, sameCars_diffID1$Typ, sameCars_diffID1$HandlerID), ]
#sameCars_diffID1<- data.table(sameCars_diffID1)

sameCars_diffID<- data.table(sameCars_diffID)
exp<- sameCars_diffID[,.(MobileID=max(MobileID), Ndays=max(Ndays), Anzeigenanlage=min(Anzeigenanlage), lastDate=max(lastDate)), 
                       by=.( Typ, Kategorie, Eigenschaften, 
                             Farbe, Kilometer, Erstzulassung, Emission, Kraftstoff, 
                             Leistung, Schaltung, Klimatisierung, Hubraum, HandlerID, 
                             LetzteAenderung, valuePrice, firstDate)]


# Merge all data rows that obviously belong to the same car:
                                  
                                  
#Step 1: Work on a small portion:
info<- sameCars_diffID$MobileID[sameCars_diffID$valuePrice==19980.1]

small<- sameCars_diffID[sameCars_diffID$MobileID %in% info,]
small<- small[order(small$HandlerID, small$Erstzulassung, small$valuePrice),]

#First attempt at merging all the values for the same cars:
#small<- data.table(small)
#sameCars_diffID<- data.table(sameCars_diffID)

#Merge car info uploaded twice the same day by mistake:

small_pure1<- exp[ ,.(Anzeigeanlage=min(Anzeigenanlage), lastDate= max(as.numeric(as.character(lastDate))), firstDate=min(as.IDate(firstDate, format="%Y-%m-%d")), 
                      Ndays=sum(Ndays), MobileID=max(MobileID), LetzteAenderung=max(as.IDate(LetzteAenderung, format="%Y-%m-%d")), erasedIDs=paste(as.character(MobileID), collapse=",") ), 
                        by=.(Typ, Kilometer, Kategorie, Eigenschaften, Farbe, Erstzulassung, 
                        Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, Hubraum, HandlerID, valuePrice)]

#hey<- sameCars_diffID[!small_pure$MobileID %in% sameCars_diffID,]
#id<- small_pure1$erasedID[small_pure1$MobileID==168704388]

#hi<- unlist(strsplit(id, ","))

#hey<- daten[daten$MobileID %in% hi, ]
#hey1<- exp[exp$MobileID %in% hi, ]
#hey2<- small_pure1[small_pure1$MobileID %in% hi, ]

small_pure1[small_pure1$valuePrice==29750 & small_pure1$handlerID==707810, ]


# Market size variable:
small_pure1<- data.table(small_pure1)
small_pure1$Erstzulassung<-paste0(as.character(small_pure1$Erstzulassung), "/01") 


small_pure1$Erstzulassung<- as.IDate(as.character(small_pure1$Erstzulassung), format="%Y/%m/%d")
small_pure1$lastDate<- as.IDate(as.character(small_pure1$lastDate), format="%Y%m%d")
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
                          & abs(data$age-data$age[i])<=60 
                          & abs(data$Kilometer-data$Kilometer[i])<=10000 
                          & data$firstDate<=data$lastDate[i]
                          & data$lastDate>= data$firstDate[i]
                          ]
  )
  )
  return(temp)
}

df<- small_pure1
rm(small_pure1)

i=112
marketSize(i, df)

xy.list<- as.list(seq(from=1, to=nrow(df), by=1))

marketSize<- rep(NA, nrow(df))
marketSize<-sapply(xy.list, FUN=marketSize, data = df)
marketSize<- as.numeric(as.character(marketSize))

df<- cbind(df, marketSize)

write.table(df, "smalldfWithMS.txt")
