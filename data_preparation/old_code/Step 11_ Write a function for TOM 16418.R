#Masterthesis:
# 1st. Advisor:       Prof. Dr. Stefan Lessmann
# Author:             Nikoleta Kovachka
# Immatriculationnr.  561099

#===============================================================================
#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
#setwd("F:\\Pkw\\MobileDaten\\")
#setwd("C:\\Users\\kovachkn.hub\\Desktop\\MA\\Pkw\\MobileDaten\\")
setwd("H:\\MA\\Pkw\\MobileDaten\\")

#=======================================================================Packages

#Load packages:
library(foreign)
#install.packages("data.table", type="source", dependencies=TRUE)
library("data.table")


#Look at these packages:
#library ( biganalytics )
#library ( bigtabulate )

#=========================================================Self-written functions

# Function to read the data and attach the date from the file name:
# parameters: filenames - name of dataframe

attachDate<- function(files){
  filenames_new<- fread(files, sep=";")
  #filenames_new<- read.table(files, sep=";")
  dates_Preise<- as.numeric(gsub("MobilePreise([0-9]+).*$", "\\1", files))
  filenames_new$Erhebungsdatum<- rep(dates_Preise, nrow(filenames_new))  
  print(paste0("The date has been merged: ", dates_Preise))
  return(filenames_new)  
}


#===============================================================================
#Part 2: Load the data
#Step 2.1: Look at the data
files.list<- list.files(pattern='MobilePreise*')

exchange.rate<- fread("eurpln.txt", sep=";")[,.(Price,Date)]
exchange.rate$Date<- as.IDate(as.character(exchange.rate$Date), format = "%Y-%m-%d")
exchange.rate$Price<- gsub(",", ".", as.character(exchange.rate$Price))
exchange.rate$Price<- as.numeric(as.character(exchange.rate$Price))
#Split the files list into sub-list for parallel computation
#ind<- rep(1:8, each = 186)
ind<- rep(1:12, each = 124)


#-------------------Brief description------------------------------------------#
# Length of total files: 1589. First: focus on price only                      #
# From 102 to 1589 - we have files for prices from every day.                  #
# Starting date - 20080918. End - 20121218.                                    #
# Length of MobilePreise files is 1488=16*3*31                                 #
# => Suggestion: split the files in 16*3=48 dataframes of length 31 days and   #
# run the code for each dataframe                                              #
#------------------------------------------------------------------------------#

#Step 2.2: Load 31 days in the console 
filenames<- files.list[1487:1488]
filenames2<- cbind(files.list, ind)
filenames2.0<- split(filenames2, ind)
#filenames3.0<- split(filenames2, ind)[10]
#filenames2.1<- split(filenames2, ind)[1]
#filenames2.2<- split(filenames2, ind)[2:3]
#filenames2.3<- split(filenames2, ind)[3]
#filenames2.4<- split(filenames2, ind)[4]

filenames2.0<- lapply(filenames2.0, "[", 1:124)
#filenames3.0<- lapply(filenames3.0, "[", 1:62)
#filenames2.2<- lapply(filenames2.2, "[", 1:186)
#filenames2.3<- lapply(filenames2.3, "[", 1:186)
#filenames2.4<- lapply(filenames2.4, "[", 1:186)

#===============================================================================BEGINNING OF FUN readData

readData<- function(filenames){
# Merge the 31 days by row:
survivalData<- do.call(`rbind`,lapply(filenames, attachDate))

setnames(survivalData, "Erhebungsdatum", "Date")
setnames(survivalData, "Preis", "valuePrice")
survivalData$Date<- as.IDate(as.character(survivalData$Date), format="%Y%m%d")
#Step 2.3: Correct coding in the merged dataframe 


#2.3a Change decimal signs, 
survivalData$valuePrice<- gsub(" ", "", survivalData$valuePrice)
survivalData$valuePrice<- gsub(",", ".", as.character(survivalData$valuePrice))

#Add VAT for netto prices:
survivalData$valuePrice[survivalData$Bemerkung=="Netto"]<- as.numeric(as.character(survivalData$valuePrice))*1.19

exchange.rate.temp<- exchange.rate[exchange.rate$Date %in% survivalData$Date, allow.cartesian=T]
#setnames(exchange.rate.temp, "Datum", "Date")
setkey(exchange.rate.temp,  "Date")
setkey(survivalData,  "Date")

survivalData<- exchange.rate.temp[survivalData,]
survivalData$valuePrice[survivalData$Bemerkung=="PLN"]<- as.numeric(as.character(survivalData$valuePrice[survivalData$Bemerkung=="PLN"]))/survivalData$Price[survivalData$Bemerkung=="PLN"]
survivalData$valuePrice<- round(as.numeric(survivalData$valuePrice), digits=0)

survivalData$Negotiable<- rep(0, nrow(survivalData))
survivalData$Negotiable[survivalData$Bemerkung=="NEGOTIABLE"]<- 1

negotiable.info<- survivalData[,c("MobileID", "valuePrice", "Negotiable"), with=F]

#Change currency to euros for PLN:

#===============================================================================
#Part 3: Create needed variables

# Step 3.1: Variable 1: Time on market:
#--------------Brief description-----------------------------------------------#
# Time on market is measured by the number of days a car is announce for sale  #
# on the website with a specific price. As soon as the price changes, the TOM  #
# is calculated for the new price from 0.                                      #
#------------------------------------------------------------------------------#

# Step 3.1.1: Count the number of days per car and price 
print("Count number of days")
#survivalData<- data.table(survivalData)
survivalData<- survivalData[,.(lastDate=max(Date), firstDate=min(Date), TOM=.N), by=.(MobileID, valuePrice)]
negotiable.info<- negotiable.info[,.(Negotiable=max(Negotiable)), by=.(MobileID, valuePrice)]



#Add a write table function here Next time :-)
print("Write file!")
write.table(survivalData, paste0("H:\\MA\\Pkw\\generatedData\\TOM\\TOM", max(filenames)), sep=";", row.names=F)
write.table(negotiable.info, paste0("H:\\MA\\Pkw\\generatedData\\TOM\\negotiable", max(filenames)), sep=";", row.names=F)

return(print("Finished!"))
}

lapply(filenames2.0[8:12], readData)
#readData(files.list)
#===============================================================================End of FUNCTION readData



#===============================================================================

#===============================Intermediary state of code (14:38 hrs, 08.04.16)
#Merge TOM-Dataset with the Specifications-dataset to calculate the market size
