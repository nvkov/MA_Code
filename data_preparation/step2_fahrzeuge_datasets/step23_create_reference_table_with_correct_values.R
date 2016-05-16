rm(list=ls())
library("data.table")
library("XML")
library("stringr")
library("pastecs")
library("qdap") #For matching and imputing

setwd("H:\\MA\\Pkw\\generatedData\\Fahrzeuge\\")
df<- fread("fahrzeugeFull_16421.txt", sep=";", 
           colClasses=c(Hubraum="numeric",Typ="factor",Leistung="numeric", Emission="numeric"))

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
df$Hubraum[as.numeric(df$Hubraum)<=1000]<- "NA"
df$Hubraum[as.numeric(df$Hubraum)>=6000]<- "NA"

#mean <- tapply(df$Hubraum,df$Typ, summary) 
#stat.desc(df[])
#Explore the missings:

#Extract complete cases:
specs<- df[complete.cases(df),]
specs<- specs[ ,.(Erstzulassung=max(Erstzulassung)), by=.(Typ, Kategorie, Emission, Kraftstoff, Leistung, Schaltung, Hubraum)]
specs$Emission[specs$Emission==""]<-"NA"
specs$Schaltung[specs$Schaltung==""]<-"NA"
specs<- specs[specs$Emission!="NA" & specs$Schaltung!="NA" &specs$Hubraum!="NA", ]
#specs<- specs[order(specs$Typ),]
specs<-specs[!duplicated(specs),]
with(specs, tapply(as.numeric(Hubraum), list(Typ), summary))

df.mini<- df[df$Hubraum=="NA", ]
df.mini<- df.mini[1:30,]
df.mini<- df.mini[,c(names(specs[,1:7, with=F])), with=F]
keycols=c("Typ", "Leistung")
setkeyv(df.mini, keycols)
setkeyv(specs, keycols)
df.mini$specs<- paste(df.mini$Typ, df$mini$Leistung, df.mini$Kraftstoff, collapse="x")
specs$specs<-paste(specs$Typ, specs$Leistung, specs$Kraftstoff, collapse="x")


df.mini$specs<- unlist(apply(df.mini[,c("Typ", "Leistung", "Kraftstoff" ), with=F], 1, paste, collapse="x"))
df.mini$specs<- gsub(" ", "", df.mini$specs)

specs$specs<- unlist(apply(specs[,c("Typ", "Leistung", "Kraftstoff" ), with=F], 1, paste, collapse="x"))
specs$specs<- gsub(" ", "", specs$specs)

df.mini$specs<- paste(df.mini$Typ, df.mini$Leistung, df.mini$Kraftstoff, collapse="x")
df.mini$Hubraum<- df.mini$Typ %l+% specs[,1:7, with=F]

#========Replace values for Hubraum:
df<- df[,Hubraum := replace(Hubraum, is.na(Hubraum), Mode(na.omit(Hubraum))), by=.(Leistung,Typ,Kategorie, Schaltung, Kraftstoff)]