rm(list=ls())
library("data.table")
project_directory<- "C:/Users/Nk/Documents/Uni/MA/"
data_directory<- "Pkw/MobileDaten/generatedData/TOM/"
setwd(paste0(project_directory, data_directory))

filenames<- list.files(pattern="TOMMobilePreise*")

tomdf<- do.call(`rbind`,lapply(filenames, fread, sep=";"))
tomdf<- tomdf[,.(TOM=sum(TOM), prices_firstDate=min(firstDate), prices_lastDate=max(lastDate)), by=.(MobileID, valuePrice, Bemerkung)]

setwd(project_directory)
sink("prices_descriptive_statistics.txt")
print("Number of unique carID/price combinations before subsetting")
nrow(tomdf)
sink()


# Remove cars with prices in Zloty: ---------------------------------------
tomdf<-tomdf[!grep("PLN", tomdf$Bemerkung),]
setnames(tomdf, "MobileID", "car_ID")
tomdf$Bemerkung<-NULL

sink("prices_descriptive_statistics.txt", append=T)
print("Number of unique carID/price combinations after removing PLN")
nrow(tomdf)
sink()

save(tomdf, file=paste0(project_directory,data_directory, "prices_df_full.RData"))

