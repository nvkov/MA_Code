#Assess full dataset:

rm(list=ls())
library("data.table")
library("sets")
library("survival")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("dataset_full.RData")

#################################################################
df$survival<- ifelse(df$right_censored==1, 0, 1)

hi<- summary(as.factor(df$Leistung))
hist(df$Leistung)

#Look at Leistung:
table(df$Leistung)

#Recode all unrealistic values to missings:
df$Leistung[df$Leistung==0]<- "NA"

df$Leistung<- as.numeric(df$Leistung)
df[,Leistung:= replace(Leistung, is.na(Leistung)==T, median(Leistung, na.rm=T)), 
   by=.(Typ, Kategorie, Schaltung, Kraftstoff, Klimatisierung, Hubraum)]

table(df$Leistung)