rm(list=ls())
library("data.table")
library("sets")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
#load("hashtable.RData")
load(paste0(project_directory,"/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step41.RData"))


##=============================================================

DOP_function<- function(df){
  modelDOP<- lm(log(df$valuePrice)~df$Erstzulassung + df$Kategorie + df$Kraftstoff) #add df$Typ for full data
  df$hedonicPrice<- predict(modelDOP, df)
  DOP<- df$valuePrice/exp(df$hedonicPrice)
  return(DOP)
}

df_merge$DOP<-DOP_function(df_merge)

#Function for splitting data on type:
uniqueTypes<- unique(df_merge$Typ)

for(i in unique(df_merge$Typ)){
  temp<- df_merge[df_merge$Typ==i,]
  save(temp, file=paste0(project_directory, data_directory, "step5/df_merge_", i, ".RData"))
print(paste0("Saved: ", i))
  }

#save(df_merge, file=paste0(project_directory, data_directory, "step5/df_merge_after_step51.RData" ))

