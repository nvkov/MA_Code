rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
load("df_step23.Rdata")

library("data.table")
library("XML")
library("stringr")
library("pastecs")
library("qdap") #For matching and imputing

#Function for mode:
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Do analysis with a smaller part of the data:
carnames<- c("C180", "C200", "C220", "E220", "E200", 
  "A180", "A160", "B180", "A170", "E320", "A150", "SLK200", "Vito", 
  "B200", "E280", "CLK200", "Viano", "ML320", "Sprinter", "E350", 
  "B170", "E270", "A140", "S320", "A200", "E250", "E240", "CLK320", 
  "ML270", "C320", "S500", "S350", "190E", "C250", "CLS350", "SL500", 
  "ML350", "B150", "SL350", "SLK230", "C270", "C240", "ML280", 
  "C230", "CLS320", "CLK230", "E300", "E230", "Vaneo", "E500", 
  "CL500", "GLK320", "GLK220", "C280", "SLK350", "B160", "C350", 
  "R320", "230E", "A190", "ML400", "CLC180", "ANDERE", "GLK350", 
  "190D", "SLK280", "CLK240", "E290", "CL180", "CLS500", "V220", 
  "CLK270", "GL320", "S400", "200D", "CL200", "R350", "SL280", 
  "SL320", "SL300", "C63AMG", "ML420", "CLK280", "CLC200", "200", 
  "CLK220", "E420", "200E", "250D", "ML500", "SL55AMG", "230CE", 
  "S600", "CL220", "R280", "CLK500", "V230", "300CE", "CLK350")

df<-df[df$Typ %in% carnames,]
sum.specs<- with(df, tapply(Hubraum, list(Typ), summary))

#unrealistic.Hubraum<- with(df, tapply(Hubraum, list(Typ), function(Hubraum) max(Hubraum[Hubraum>quantile(Hubraum, na.rm=T)[3]])) 


df$Erstzulassung<-paste0(as.character(df$Erstzulassung), "/01") 
df$Erstzulassung<- as.IDate(as.character(df$Erstzulassung), format="%Y/%m/%d")
df$Hubraum<- as.numeric(as.character(df$Hubraum))
sort(unique(df$Hubraum))
#Erase unrealistic Hubraum:
df$Hubraum[as.numeric(df$Hubraum)<=1000]<- NA
df$Hubraum[as.numeric(df$Hubraum)>=6000]<- NA

#See again what is left:
sum.specs<- with(df, tapply(as.numeric(Hubraum), list(Typ), summary))


#mean <- tapply(df$Hubraum,df$Typ, summary) 
#stat.desc(df[])
#Explore the missings:

#Extract complete cases:
specs<- df[complete.cases(df),]
specs<- specs[ ,.(Erstzulassung=max(Erstzulassung)), by=.(Typ, Kategorie, Emission, Kraftstoff, Leistung, Schaltung, Hubraum)]

specs$Emission[specs$Emission==""]<-NA
#specs$Emission<- as.numeric(as.character(specs$Emission))

specs$Schaltung[specs$Schaltung==""]<-NA
#specs$Emission<- as.numeric(as.character(specs$Schaltung))

q25Leistung<- quantile(df$Leistung, 0.25, na.rm=T)
specs$Leistung[specs$Leistung<q25Leistung]<-NA

specs<- specs[ ,.(Erstzulassung=max(Erstzulassung)), by=.(Typ, Kategorie, Emission, Kraftstoff, Leistung, Schaltung, Hubraum)]
specs<- specs[!is.na(specs$Emission) & !is.na(specs$Schaltung) & !is.na(specs$Hubraum), ]
specs<- specs[!is.na(specs$Erstzulassung) & !is.na(specs$Leistung) & !is.na(specs$Kraftstoff), ]

specs<-specs[!duplicated(specs),]
with(specs, tapply(as.numeric(Hubraum), list(Typ), summary))
with(specs, tapply(as.Date(Erstzulassung), list(Typ), summary))
with(specs, tapply(as.factor(Kraftstoff), list(Typ), summary))
with(specs, tapply(as.factor(Kategorie), list(Typ), summary))

#Round up Hubraum to the tens:
specs$HubraumRound<- round(specs$Hubraum/10)*10
with(specs, tapply(as.numeric(Hubraum), list(Typ), summary))
with(specs, tapply(as.numeric(HubraumRound), list(Typ), summary))

<<<<<<< HEAD
specs<- specs[ ,.(Erstzulassung=max(Erstzulassung)), by=.(Typ, Kategorie, Emission, Kraftstoff, Leistung, Schaltung, HubraumRound)]
specsA180<-specs[specs$Typ=="A180"]

=======
>>>>>>> 8457b2946a54de64e3b8de7bdfe165cdbcd2f4b3
save(specs, file=paste0(wd, "hashtable.Rdata"))


