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

#Clean up cars with two IDs but the same characteristics:

# Find observations with perfect repetition on relevantCols:
relevantCols<- c( "valuePrice", "Kilometer", "Typ", 
                 "Kategorie", "Erstzulassung", "Emission", "Kraftstoff", "Leistung", 
                 "Schaltung", "HU", "Klimatisierung", "Hubraum", "HandlerID" )


# Extract all the cars with the identified IDs:
dupID<- unique(subsub$MobileID)
sameCars_diffID<- daten[daten$MobileID %in% dupID, ]

# Merge all data rows to extract leasing traders:
exp<- daten[,.(MobileID=max(MobileID), TOM=max(TOM), 
                         Anzeigenanlage=min(Anzeigenanlage), lastDate=max(lastDate),
                         erasedIDs=paste(as.character(MobileID), collapse=","),
                         leasingCount=length(unique(MobileID))), 
                       
                      by=.( Typ, Kategorie,Kilometer, Erstzulassung, Emission, Kraftstoff, 
                             Leistung, Schaltung, Klimatisierung, Hubraum, HandlerID, 
                             LetzteAenderung, valuePrice, firstDate)]

# Extract vector with leasing cars:

leasingDF<- exp[exp$leasingCount>2,]
leasingID<- leasingDF$erasedIDs

# Make a vector that indicates leasingIDs
leasingID<- split(leasingID, sep=",")

datenLeasing<- daten[daten$MobileID %in% leasingID,]
#Take away leasing IDs from the calculation
datenNonLeasing[!daten$MobileID %in% leasingID,]
                  
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
#small_pure1<- data.table(small_pure1)
small_pure1$Erstzulassung<-paste0(as.character(small_pure1$Erstzulassung), "/01") 
small_pure1$Erstzulassung<- as.IDate(as.character(small_pure1$Erstzulassung), format="%Y/%m/%d")
small_pure1$Anzeigeanlage<- as.IDate(as.character(small_pure1$Anzeigeanlage), format="%Y%m%d")
small_pure1$age<- difftime(small_pure1$lastDate, small_pure1$Erstzulassung, units="days")
#small_pure1$interval<- integers(as.numeric(small_pure1$LetzteAenderung), as.numeric(small_pure1$lastDate))


daten<- small_pure1
save(daten, file=paste0(wd, "clean_betaset_merge.Rdata"))

