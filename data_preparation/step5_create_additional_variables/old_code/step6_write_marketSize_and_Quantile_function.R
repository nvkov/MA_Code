rm(list=ls())
library("data.table")
library("sets")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)
#load("hashtable.RData")
load(paste0(project_directory,"/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step41_A.RData"))

marketSize<- function(i, data){
  temp<- length(unique(
    
    data$car_ID[ data$Typ==data$Typ[i] 
                          & data$Kategorie==data$Kategorie[i] 
                          & abs(data$age-data$age[i])<=90 
                          & abs(data$Kilometer-data$Kilometer[i])<=10000 
                          & data$prices_firstDate<=data$prices_lastDate[i]
                          & data$prices_lastDate>= data$prices_firstDate[i]
                          ]
  )
  )
  
  temp2<- ecdf(data$valuePrice[data$Typ==data$Typ[i] 
                   & data$Kategorie==data$Kategorie[i] 
                   & abs(data$age-data$age[i])<=90 
                   & abs(data$Kilometer-data$Kilometer[i])<=10000 
                   & data$prices_firstDate<=data$prices_lastDate[i]
                   & data$prices_lastDate>= data$prices_firstDate[i]
                   ]
  )(data$valuePrice[i])
  
  
  
  print(i)
  return(cbind(temp, temp2))
}


i=1260
data<-df_merge_A
marketSize(i, data)

 #mS<- as.numeric(as.character(mS))

df<- cbind(data, mS)
save(df, file=paste0(wd,"msdf.RData"))




