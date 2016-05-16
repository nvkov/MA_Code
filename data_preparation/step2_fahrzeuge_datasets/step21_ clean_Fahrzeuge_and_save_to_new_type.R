#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
files.list.fahrzeuge<- list.files(pattern='MobileFahrzeuge([0-9]+)Orig*')
length(files.list.fahrzeuge)
files.list<- files.list.fahrzeuge[1]

#Merge and purge data:

#write function to read Fahrzeuge
readFahrzeuge<-function(files.list){
df<- fread(files.list, sep=";")
df<- df[df$Art=="USED",]
setnames(df,names(df)[21],"HandlerID")
print("Finished reading")

#Clean up the form for Erstzulassung
df$Erstzulassung<- as.character(df$Erstzulassung)
df$Erstzulassung<-str_extract(df$Erstzulassung, "[0-9]{4}/[0-9]{2}")
print("Cleaned up Erstzulassung")

#Clean up data from the @
df$Eigenschaften<- gsub("@", "", df$Eigenschaften)
print("Cleaned up Eigenschaften")

df$Farbe<- gsub("@", "", df$Farbe)
print("Cleaned up Farbe")

df$Emission<- gsub("@", "", df$Emission)
print("Cleaned up Emission")

df$Kraftstoff<- gsub("@", "", df$Kraftstoff)
print("Cleaned up Kraftstoff")

df$Schaltung<- gsub("@", "", df$Schaltung)
print("Cleaned up Schaltung")

df$Klimatisierung<- gsub("@", "", df$Klimatisierung)
print("Cleaned up Klimetisierung")

df$Kategorie<- gsub("@", "", df$Kategorie)

df$TypOrig<-df$Typ
print("Cleaned up Kategorie")

# See which names are already present in the dataset:
validnames<-unique(df$Typ)
validnames<- gsub(" ", "", as.character((validnames)))

#Correct names of cars registered as invalid Type name:

#Extract all models with fixed names:

df.number<- df[grep("^[0-9]{3}", as.character(df$Typ)), ]

df.number$Typ<- as.character(df.number$Typ)
condition<-"(Vito|Sprinter|Ponton|Adenauer|PONTON|SL 55 AMG|SL55AMG|ML55 AMG|S 55 AMG|220/8|230.4|CL63AMG|ML63AMG|250/8|230.6)" 

df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition))==F]<- str_extract(df.number$Beschreibung, condition)[is.na(str_extract(df.number$Beschreibung, condition))==F]

df$Typ[grep("^[0-9]{3}", as.character(df$TypOrig))]<- df.number$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)

#-------------------------------------------------------------------------------
df.number<- df[grep("^[0-9]{3}", as.character(df$Typ)), ]


df.number$Typ<- as.character(df.number$Typ)
condition1<-"(CL|MB|S|D|SL|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE) [0-9]{3}" 
condition2<-"(CL|MB|S|D|SL|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE)[0-9]{3}"   

df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition1))==F]<- str_extract(df.number$Beschreibung, condition1)[is.na(str_extract(df.number$Beschreibung, condition1))==F]
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition2))==F]<- str_extract(df.number$Beschreibung, condition2)[is.na(str_extract(df.number$Beschreibung, condition2))==F]

df$Typ[grep("^[0-9]{3}", as.character(df$Typ))]<- df.number$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)

#--------Second clean:
df.number<- df[grep("^[0-9]{3}", as.character(df$Typ)), ]
df.number$Typ<- as.character(df.number$Typ)

condition3<- "[0-9]{3} (MB|S|SL|D|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE) "
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition3))==F]<- str_extract(df.number$Beschreibung, condition3)[is.na(str_extract(df.number$Beschreibung, condition3))==F]

condition4<- "[0-9]{3}(MB|S|SL|D|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE) "
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition4))==F]<- str_extract(df.number$Beschreibung, condition4)[is.na(str_extract(df.number$Beschreibung, condition4))==F]


df$Typ[grep("^[0-9]{3}", as.character(df$Typ))]<- df.number$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)

#Third Clean=================================

df.number<- df[grep("^[0-9]{3}", as.character(df$Typ)), ]
df.number$Typ<- as.character(df.number$Typ)

condition5<- "[0-9]{3} (MB|S|SL|D|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE)"
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition5))==F]<- str_extract(df.number$Beschreibung, condition5)[is.na(str_extract(df.number$Beschreibung, condition5))==F]

condition6<- "[0-9]{3}(MB|S|SL|D|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE)"
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition6))==F]<- str_extract(df.number$Beschreibung, condition6)[is.na(str_extract(df.number$Beschreibung, condition6))==F]


df$Typ[grep("^[0-9]{3}", as.character(df$Typ))]<- df.number$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)


#Fourth clean: Get the class type of the rest:
df.number<- df[grep("^[0-9]{3}", as.character(df$Typ)), ]
df.number$Typ<- as.character(df.number$Typ)

condition7<- "[A-Z]{1,3}(-| )Klasse"
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition7))==F]<- paste0(str_extract(df.number$Beschreibung, condition7)[is.na(str_extract(df.number$Beschreibung, condition7))==F], df.number$TypOrig[is.na(str_extract(df.number$Beschreibung, condition7))==F])

condition8<- "AMG"
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition8))==F]<- str_extract(df.number$Beschreibung, condition8)[is.na(str_extract(df.number$Beschreibung, condition8))==F]


df$Typ[grep("^[0-9]{3}", as.character(df$Typ))]<- df.number$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)
df$Typ<- gsub("Klasse", "", df$Typ)
df$Typ<- gsub("-", "", df$Typ)

# AMG Full name:
#Fourth clean: Get the class type of the rest:
df.number<- df[df$Typ=="AMG", ]
df.number$Typ<- as.character(df.number$Typ)

condition7<- "(55|65|63|30)"
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition7))==F]<- paste0(str_extract(df.number$Beschreibung, condition7)[is.na(str_extract(df.number$Beschreibung, condition7))==F], "AMG")
df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition7))==F]<- paste0(str_extract(df.number$Beschreibung, "ML|E|C|CLK|G|S|CLS|SL")[is.na(str_extract(df.number$Beschreibung, condition7))==F], df.number$Typ[is.na(str_extract(df.number$Beschreibung, condition7))==F])

print("Cleaned up Number names")

#================================================================================
#Correct names of cars registered as ANDERE:
#Extract all models with fixed names:

df.andere<- df[grep("ANDERE", as.character(df$Typ)), ]

df.andere$Typ<- as.character(df.andere$Typ)
condition<-"(Vito|Sprinter|Ponton|Adenauer|PONTON|SL 55 AMG|SL55AMG|ML55 AMG|S 55 AMG|220/8|230.4|CL63AMG|ML63AMG|250/8|230.6|200/8)" 

df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition))==F]<- str_extract(df.andere$Beschreibung, condition)[is.na(str_extract(df.andere$Beschreibung, condition))==F]

df$Typ[grep("ANDERE", as.character(df$Typ))]<- df.andere$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)

#-------------------------------------------------------------------------------
df.andere<- df[grep("ANDERE", as.character(df$Typ)), ]

df.andere$Typ<- as.character(df.andere$Typ)
condition1<-"(CL|MB|S|D|SL|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE) [0-9]{3}" 
condition2<-"(CL|MB|S|D|SL|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE)[0-9]{3}"   

df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition1))==F]<- str_extract(df.andere$Beschreibung, condition1)[is.na(str_extract(df.andere$Beschreibung, condition1))==F]
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition2))==F]<- str_extract(df.andere$Beschreibung, condition2)[is.na(str_extract(df.andere$Beschreibung, condition2))==F]

df$Typ[grep("ANDERE", as.character(df$Typ))]<- df.andere$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)

#--------Second clean:
df.andere<- df[grep("ANDERE", as.character(df$Typ)), ]
df.andere$Typ<- as.character(df.andere$Typ)

condition3<- "[0-9]{3} (MB|S|SL|D|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE) "
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition3))==F]<- str_extract(df.andere$Beschreibung, condition3)[is.na(str_extract(df.andere$Beschreibung, condition3))==F]

condition4<- "[0-9]{3}(MB|S|SL|D|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE) "
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition4))==F]<- str_extract(df.andere$Beschreibung, condition4)[is.na(str_extract(df.andere$Beschreibung, condition4))==F]


df$Typ[grep("ANDERE", as.character(df$Typ))]<- df.andere$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)

#Third Clean=================================

df.andere<- df[grep("ANDERE", as.character(df$Typ)), ]
df.andere$Typ<- as.character(df.andere$Typ)

condition5<- "[0-9]{3} (MB|S|SL|D|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE)"
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition5))==F]<- str_extract(df.andere$Beschreibung, condition5)[is.na(str_extract(df.andere$Beschreibung, condition5))==F]

condition6<- "[0-9]{3}(MB|S|SL|D|SLC|SE|SEL|E|C|M|A|ML|SLK|CLS|CLK|S|GL|AMG|TE|CE|SEC|TE|GE)"
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition6))==F]<- str_extract(df.andere$Beschreibung, condition6)[is.na(str_extract(df.andere$Beschreibung, condition6))==F]


df$Typ[grep("ANDERE", as.character(df$Typ))]<- df.andere$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)


#Fourth clean: Get the class type of the rest:
df.andere<- df[grep("ANDERE", as.character(df$Typ)), ]
df.andere$Typ<- as.character(df.andere$Typ)

condition7<- "[A-Z]{1,3}(-| )Klasse"
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition7))==F]<- str_extract(df.andere$Beschreibung, condition7)[is.na(str_extract(df.andere$Beschreibung, condition7))==F]

condition8<- "AMG"
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition8))==F]<- str_extract(df.andere$Beschreibung, condition8)[is.na(str_extract(df.andere$Beschreibung, condition8))==F]


df$Typ[grep("ANDERE", as.character(df$Typ))]<- df.andere$Typ

df$Typ<- gsub(" ", "", df$Typ)
df$Typ<- gsub("NA", "", df$Typ)
df$Typ<- gsub("Klasse", "", df$Typ)
df$Typ<- gsub("-", "", df$Typ)

# AMG Full name:
#Fourth clean: Get the class type of the rest:
df.andere<- df[df$Typ=="AMG", ]
df.andere$Typ<- as.character(df.andere$Typ)

condition7<- "(55|65|63|30)"
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition7))==F]<- paste0(str_extract(df.andere$Beschreibung, condition7)[is.na(str_extract(df.andere$Beschreibung, condition7))==F], "AMG")
df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition7))==F]<- paste0(str_extract(df.andere$Beschreibung, "ML|E|C|CLK|G|S|CLS|SL")[is.na(str_extract(df.andere$Beschreibung, condition7))==F], df.andere$Typ[is.na(str_extract(df.andere$Beschreibung, condition7))==F])
print("Cleaned up ANDERE")
#------------------------------------------------------------------------------------------------------



df<- df[, (maxDatum=max(Datum)), 
        by=.(MobileID, Anzeigenanlage, LetzteAenderung, Typ, Kategorie,
             Eigenschaften, Farbe,  Kilometer,  HU,  Erstzulassung,  Emission,  Kraftstoff,  
             Leistung, Schaltung,  Klimatisierung, Hubraum,  HandlerID) ] 
print("Dropped Beschreibung")

#Write table to file
write.table(df, paste0(project_directory, data_directory, "/generatedData/CleanFahrzeuge/Clean", max(files.list) ), row.names = F, sep=";")
print("File written")
#return(df)
return(print("Done!"))
}

#readFahrzeuge(files.list)
lapply(files.list.fahrzeuge[1:47], readFahrzeuge)

#-------------------------------------------------------------------------------

