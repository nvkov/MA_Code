rm(list=ls())

#Set working directory
project_directory<- getwd()
data_directory<-"/Pkw/MobileDaten/"
setwd(paste0(project_directory))


#=======================================================================Packages

#Load packages:
library(foreign)
library("data.table")

#Look at these packages:
#library ( biganalytics )
#library ( bigtabulate )

#=========================================================Self-written functions

# Function to read the data and attach the date from the file name:

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

#Split the files list into sub-list for parallel computation
#ind<- rep(1:8, each = 186)
ind<- rep(1:12, each = 124)


#Step 2.2: Load 31 days in the console 
filenames<- files.list[1487:1488]
filenames2<- cbind(files.list, ind)
filenames2.0<- split(filenames2, ind)
filenames2.0<- lapply(filenames2.0, "[", 1:124)

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
survivalData$valuePrice<- as.numeric(survivalData$valuePrice)

#Add VAT for netto prices:
survivalData$valuePrice[survivalData$Bemerkung=="Netto"]<-survivalData$valuePrice[survivalData$Bemerkung=="Netto"]*1.19

#Change currency to euros for PLN:

# Step 3.1.1: Count the number of days per car and price 
print("Count number of days")
survivalData<- survivalData[,.(lastDate=max(Date), firstDate=min(Date), TOM=.N), by=.(MobileID, valuePrice, Bemerkung)]

print("Write file!")
write.table(survivalData, paste0(project_directory, "/generatedData/TOM/TOM", max(filenames)), sep=";", row.names=F)
return(print("Finished!"))
}

lapply(filenames2.0[1:12], readData)
#readData(files.list)
#===============================================================================End of FUNCTION readData



#===============================================================================

#===============================Intermediary state of code (14:38 hrs, 08.04.16)
#Merge TOM-Dataset with the Specifications-dataset to calculate the market size
