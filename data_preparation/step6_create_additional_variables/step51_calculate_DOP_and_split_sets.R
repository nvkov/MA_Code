rm(list=ls())
library("data.table")
library("sets")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
#load("hashtable.RData")
load(paste0(project_directory,"/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step51.RData"))


##=============================================================

df$age<- as.numeric(difftime(df$prices_lastDate, df$Erstzulassung, units = "days"))
df$Kraftstoff<- as.factor(df$Kraftstoff)
df$Kategorie<- as.factor(df$Kategorie)
df$Typ<- as.factor(df$Typ)
# Luxury extras -----------------------------------------------------------

df$Leather_seats<- grepl("26",df$Eigenschaften)
df$Full_service_history<- grepl("23",df$Eigenschaften)
df$Four_wheel_drive<- grepl("22",df$Eigenschaften)
df$Xenon_lights<- grepl("37",df$Eigenschaften)


# DOP ---------------------------------------------------------------------

DOP_function<- function(df){
  modelDOP<- lm(log(df$valuePrice)~df$Erstzulassung + df$Kategorie + df$Kraftstoff + 
                  df$age + df$Typ + df$Leistung) #add df$Typ for full data
  df$hedonicPrice<- predict(modelDOP, df)
  DOP<- df$valuePrice/exp(df$hedonicPrice)
  return(DOP)
}

df$DOP<-DOP_function(df)



# Split data --------------------------------------------------------------


#Function for splitting data on type:
uniqueTypes<- unique(df$Typ)

for(i in unique(df$Typ)){
  temp<- df[df$Typ==i,]
  save(temp, file=paste0(project_directory, data_directory, "step6/df_merge_", i, ".RData"))
print(paste0("Saved: ", i))
  }

#save(df_merge, file=paste0(project_directory, data_directory, "step5/df_merge_after_step51.RData" ))

