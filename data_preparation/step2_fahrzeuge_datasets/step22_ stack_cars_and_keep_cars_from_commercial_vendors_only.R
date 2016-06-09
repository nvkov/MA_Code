#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
files.list.fahrzeuge<- list.files(pattern='CleanMobileFahrzeuge([0-9]+)Orig*')
length(files.list.fahrzeuge)
files.list<- files.list.fahrzeuge[1]
load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Vendors/vendor_df.RData")
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

df<- df[vendor_df$vendor_ID %in% df$HandlerID,]

print(paste0("Number of rows is: ", nrow(df)))
save(df, file=paste0(wd,"/cars_full_after_step22.RData" ))
print("File written")
return(print("Done!"))
}


df<- readFahrzeugeClean(files.list.fahrzeuge)


#-------------------------------------------------------------------------------
