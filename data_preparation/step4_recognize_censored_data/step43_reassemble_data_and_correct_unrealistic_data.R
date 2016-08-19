# Correct unrealistic values:

# Prices and Kms

rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<- "/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("Merged_data/df_merge_after_step41.RData")
load("Merged_data/leasing_after_step42.Rdata")
load("Merged_data/overlaps_after_step42.Rdata")


# Keep only relevant vars -------------------------------------------------
relCols<- c("car_ID", "vendor_ID",  
            "valuePrice", "TOM", 
            "prices_firstDate", "prices_lastDate", 
            "cars_lastChange", "cars_lastDate",
            "Anzeigenanlage", "Typ", "Farbe", "Kilometer", "HU", "Erstzulassung", 
            "Emission", "Kraftstoff", "Leistung", "Schaltung", "Klimatisierung", 
            "Hubraum", "Eigenschaften", "Kategorie", "right_censor")

vendors<- vendors[,relCols, with=F]
overlaps<- overlaps[,relCols, with=F]
leasing<- leasing[,relCols, with=F]

df<- rbind(vendors, overlaps, leasing)
rm(vendors, leasing, overlaps)

# Look at values ----------------------------------------------------------

summary(df$Kilometer)
summary(df$valuePrice)

# Drop unrealistic prices -------------------------------------------------

View(df[df$valuePrice<1000,])
View(df[df$valuePrice>150000,])

#Unrealistic categories:

#Low or too high values
df$vendor_ID[df$valuePrice<1000]<-NA
df$vendor_ID[df$valuePrice>=999999]<-NA

#Mistake was corrected at later stage
df$vendor_ID[df$valuePrice>150000 & df$right_censor==1]<-NA
df$vendor_ID[df$valuePrice>150000 & df$TOM==1]<-NA

df<- df[!is.na(df$vendor_ID),]

#Mistake due to an additional 0:
df$Sclass<- rep(0, nrow(df))
df$Sclass[grep("S", df$Typ)]<- 1

df$valuePrice1<- df$valuePrice/10
df$valuePrice[df$valuePrice>=100000 & df$Sclass==0]<-df$valuePrice1[df$valuePrice>=100000 & df$Sclass==0]



# Find the mean average deviation and detect other outliers by group ----------

df[,mad:=abs(valuePrice - median(valuePrice)) / mad(valuePrice, constant=1), by=(Typ)]

# Look at prices per car type -------------------------------------------------

for(i in unique(df$Typ)){
  print(i)
  print(summary(df$valuePrice[df$Typ==i]))
}



save(df, file="Merged_data/df_merge_after_step43.RData")
