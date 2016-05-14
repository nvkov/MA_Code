#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
setwd("H:\\MA\\Pkw\\generatedData\\Fahrzeuge\\")


library("data.table")
library("stringr")

#Read files with car specifications:
files.list.fahrzeuge<- list.files(pattern='ClearMobileFahrzeuge([0-9]+)Orig*')
length(files.list.fahrzeuge)
memory.limit(size=50000)

#Merge clean data:

#write function to read Fahrzeuge
readFahrzeugeClean<-function(files.list){
df<- data.table(do.call(`rbind`,lapply(files.list, fread, sep=";")))

df<- df[, .(maxDatum=max(V1)), 
                  by=.(MobileID, Anzeigenanlage, LetzteAenderung, Typ, Kategorie,
                  Eigenschaften, Farbe,  Kilometer,  HU,  Erstzulassung,  Emission,  Kraftstoff,  
                  Leistung, Schaltung,  Klimatisierung, Hubraum,  HandlerID) ] 

print("Finished reading")

df<- df[, .(Eigenschaften= paste(Reduce(intersect, strsplit(Eigenschaften, ",") ), collapse=","), LetzteAenderung=max(LetzteAenderung), maxDatum=max(maxDatum)), 
                                   by=.(MobileID, Kilometer, Anzeigenanlage, Typ, Kategorie, Farbe, Erstzulassung,
                                        Emission, Kraftstoff, Leistung, Schaltung, HU, Klimatisierung,Hubraum, HandlerID)]

print("Reduced Eigenschaften")

print(paste0("Number of rows is: ", nrow(df)))

write.table(df, paste0("H:\\MA\\Pkw\\generatedData\\fahrzeugeFull_16421.txt" ),row.names = F, sep=";" )
print("File written")
return(print("Done!"))
}


df<- readFahrzeugeClean(files.list.fahrzeuge)


#-------------------------------------------------------------------------------
