#Descriptive stats -Empirical study:
#Correct Leistung:

rm(list=ls())
library("data.table")
library("sets")
library("survival")
library("stargazer")
library("beepr")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("dataset_full.RData")

df$newTOM<- as.numeric(df$prices_lastDate-df$prices_firstDate) +1
df$status<- abs(df$right_censor - 1)
df$year_bought<-as.factor(format(df$prices_lastDate, "%Y")) 

# Survival models ---------------------------------------------------------
relCols<- c("valuePrice", "newTOM", "MS","DOP", "Quantile", "age", "Leather_seats", 
            "Full_service_history", "Xenon_lights", "color_cat", "year_bought", 
            "Leistung", "Typ","status", "vendor_ID", "car_ID", "prices_firstDate", "prices_lastDate", "Erstzulassung") 
df1<- df[df$DOP<=2 & df$newTOM<400,relCols, with=F]
df1$newDOP<- df1$DOP*10

# Generate some vendor profile variables ----------------------------------

df1<- df1[ ,size_vendor:=length(unique(car_ID)), by=vendor_ID]
df1$size_vendor<- df1$size_vendor/100

df1<- df1[,times_present_diff_price:=1:.N, by=car_ID]
df1<- d1[, total_price_reduction]
df1$valuePrice100<- df1$valuePrice/100
df1$Class<- gsub("[0-9]", "" ,df1$Typ)
df1$Hub_Cat<- gsub("[A-Z]", "", df1$Typ)


# Desriptive stats --------------------------------------------------------

#AClass
length(unique(df1$car_ID[df1$Class=="A" & df1$Erstzulassung<=as.numeric(as.Date("2004-11-01"))]))

length(unique(df1$car_ID[df1$Class=="A" & df1$Erstzulassung>as.numeric(as.Date("2004-11-01"))]))
length(unique(df1$car_ID[df1$Class=="B"]))

length(unique(df1$car_ID[df1$Class=="E" & df1$Erstzulassung>as.numeric(as.Date("2009-07-01"))]))
length(unique(df1$car_ID[df1$Class=="E" & df1$Erstzulassung<=as.numeric(as.Date("2002-07-01"))]))

