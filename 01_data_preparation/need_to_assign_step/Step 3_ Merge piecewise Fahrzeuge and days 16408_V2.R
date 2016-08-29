#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
setwd("H:\\MA\\Pkw\\MobileDaten\\ClearFahrzeugeData\\")


library("data.table")
library("stringr")

cardf<- fread("H:\\MA\\Pkw\\MobileDaten\\ClearFahrzeugeData\\CleanReducedfahrzeugeFull_16405.txt", sep=";")
tomdf<- fread("H:\\MA\\Pkw\\generatedData\\TOM\\TOMmerge.txt", sep=";") 

setkey(cardf, "MobileID")
setkey(tomdf, "MobileID")

minimerge<- cardf[tomdf, allow.cartesian=TRUE]
rm(cardf, tomdf)
minimerge<- minimerge[maxDatum>=firstDate & LetzteAenderung<=lastDate,]



mergedf<- minimerge[, !c("LetzteAenderung", "maxDatum"), with=F ]
#eigdf<- minimerge[, c("MobileID", "firstDate", "lastDate", "HandlerID", "Eigenschaften", "LetzteAenderung", "maxDatum"), with=F]

#write.table(eigdf, "H:\\MA\\Pkw\\MobileDaten\\ClearFahrzeugeData\\Eigenschaften.txt", sep=";", row.names=F)
#write.table(mergedf, "H:\\MA\\Pkw\\generatedData\\firstMerge.txt", sep=";", row.names=F)


#rm(minimerge, eigdf)
rm(minimerge)

# Find duplicated data with different MobileIDs:
#dups<- duplicated(mergedf[ , c("Typ", "Kategorie", "Eigenschaften", "Kilometer", "Erstzulassung",
                               "Emission", "Kraftstoff", "Schaltung", "HandlerID", "valuePrice"), with=F])

#subsub<- mergedf[dups & !duplicated(mergedf$MobileID), ]


# Extract all the cars with the identified IDs:
#dupID<- unique(subsub$MobileID)
#sameCars_diffID<- mergedf[mergedf$MobileID %in% dupID, ]

# Drop duplicated entries put online on the same day by mistake:
mergedf<- mergedf[,.(MobileID=max(MobileID), TOM=max(TOM), Anzeigenanlage=min(Anzeigenanlage), lastDate=max(lastDate)), 
                      by=.( Typ, Kategorie, Eigenschaften, 
                            Farbe, Kilometer, Erstzulassung, Emission, Kraftstoff, HU, 
                            Leistung, Schaltung, Klimatisierung, Hubraum, HandlerID, 
                            valuePrice, firstDate)]

# Merge entries uploaded consecutively several times with different MobileIDs:
#Merge car info uploaded twice the same day by mistake:

mergedf<- mergedf[ ,.(lastDate= max(lastDate), firstDate=min(firstDate), 
                      TOM=sum(TOM), MobileID=max(MobileID), erasedIDs=paste(as.character(MobileID), collapse=","), IDChangeCount=.N ), 
                  by=.(Typ, Kilometer, Kategorie, Eigenschaften, Farbe, Erstzulassung, HU, 
                       Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, Hubraum, HandlerID, valuePrice)]

mergedf<- mergedf[, !"Eigenschaften", with=F ]
mergedf<- mergedf[, !"erasedIDs", with=F ]


write.table(mergedf, "H:\\MA\\Pkw\\generatedData\\finalMerge16408.txt", sep=";", row.names=F)

#Problem: too many erased: see where is mistake - 43551828,83237175,83396201,84653518
#---> As I can see, apparently there is no mistake
hi<- mergedf[mergedf$MobileID %in% c(43568870,83364029,84926847,85085308,86527368,86914374,88373657,89253470,90656193,92241674,92925234,93754072,95068603,96164093,97460841,98145231,98864281, 93754072,95068603,96164093,97460841,98145231,98864281)]
View(hi)
hi1<- df[df$MobileID==98864281,]
View(hi1)

#Write a variable for the distance between Erstzulassung and HU:

df$Erstzulassung<-paste0(as.character(df$Erstzulassung), "/01") 
df$Erstzulassung<- as.IDate(as.character(df$Erstzulassung), format="%Y/%m/%d")

df$HU<-paste0(as.character(df$HU), "/01") 
df$HU<- as.IDate(as.character(df$HU), format="%Y/%m/%d")
df$lastDate<- as.IDate(as.character(df$lastDate), format="%Y%m%d")
df$firstDate<- as.IDate(as.character(df$firstDate), format="%Y%m%d")

df$age<- difftime(df$lastDate, df$Erstzulassung, units="days")

#It is important to calculate this after merging all datasets!
df$HUEZdiff<- difftime(df$HU, df$Erstzulassung, units="days")

#Calculate Market size:
data<- data.table(df)

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

df<- data.table(df)
#xy.list<- as.list(seq(from=1, to=nrow(df), by=1))
xy.list<- as.list(seq(from=1, to=3, by=1))



marketsize<-sapply(xy.list, FUN=marketSize, data = df)
marketsize<- as.numeric(as.character(marketsize))

df<- cbind(df, marketSize)
