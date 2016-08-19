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

# Look at values ----------------------------------------------------------

summary(vendors$Kilometer)
summary(vendors$valuePrice)

# Drop unrealistic prices -------------------------------------------------

#View(vendors[vendors$valuePrice<1000,])
#View(vendors[vendors$valuePrice>150000,])

#Unrealistic categories:

#Low or too high values
vendors[vendors$valuePrice<1000,]<-NA
vendors[vendors$valuePrice>=999999,]<-NA

#Mistake was corrected at later stage
vendors[vendors$valuePrice>150000 & vendors$right_censor==1,]<-NA
vendors[vendors$valuePrice>150000 & vendors$TOM==1,]<-NA

vendors<- vendors[!is.na(vendors$vendor_ID),]

#Mistake due to an additional 0:
vendors$Sclass<- rep(0, nrow(vendors))
vendors$Sclass[grep("S", vendors$Typ)]<- 1

vendors$valuePrice1<- vendors$valuePrice/10
vendors$valuePrice[vendors$valuePrice>=100000 & vendors$Sclass==0]<-vendors$valuePrice1[vendors$valuePrice>=100000 & vendors$Sclass==0]


# Look at prices per car type -------------------------------------------------

for(i in unique(vendors$Typ)){
  print(i)
  print(summary(vendors$valuePrice[vendors$Typ==i]))
}



save(vendors, file="Merged_data/df_merge_after_step43.RData")
