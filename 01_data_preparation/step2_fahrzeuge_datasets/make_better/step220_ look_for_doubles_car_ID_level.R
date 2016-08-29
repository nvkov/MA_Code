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
vendors<- load(paste0(project_directory, "/Pkw/MobileDaten/generatedData/Vendors/vendor_df.RData"))
#Merge clean data:

#write function to read Fahrzeuge
readFahrzeugeClean<-function(files.list){
df<- data.table(do.call(`rbind`,lapply(files.list, fread, sep=";")))

#Subset for relevant vendors:
setnames(df, "HandlerID", "vendor_ID")
setkeyv(df, "vendor_ID")
setkeyv(vendor_df, "vendor_ID")

df<- df[as.numeric(as.character(df$vendor_ID)) %in% as.numeric(as.character(vendor_df$vendor_ID)),]

save(df, file=paste0(wd,"/cars_commercial_vendors_full.RData" ))
print("File written")
return(print("Done!"))
}


df<- readFahrzeugeClean(files.list.fahrzeuge)


#-------------------------------------------------------------------------------
