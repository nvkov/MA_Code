rm(list=ls())
library("data.table")
library("sets")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/step5/"
wd<- paste0(project_directory, data_directory)

setwd(wd)


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

xy.list<- as.list(seq(from=1, to=nrow(data), by=1))

mS<- rep(NA, nrow(data))
mS<-t(sapply(xy.list, FUN=marketSize, data = data))
#mS<- as.numeric(as.character(mS))

#df<- cbind(data, mS)
save(df, file=paste0(wd,"msdf.RData"))

files.list<-list.files()
for(i in files.list){
  load(paste0(project_directory, data_directory, i))
  print(i)
  xy.list<- as.list(seq(from=1, to=nrow(temp), by=1))
      mS<- rep(NA, nrow(temp))
  mS<-t(sapply(xy.list, FUN=marketSize, data = temp))
  temp<- cbind(temp, mS)
  setnames(temp, "V1", "MS")
  setnames(temp, "V2", "Quantile")
  save(temp, file=paste0(project_directory, data_directory, "new_", i ))
}
##=============================================================
# Survival data:
#library(survival)
#mini.surv <- survfit(Surv(df$TOM)~ df$mS, conf.type="none")
#summary(mini.surv)
#plot(mini.surv, xlab="Time", ylab="Survival Probability")


