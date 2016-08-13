#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
setwd("H:\\MA\\Pkw\\generatedData\\")


library("data.table")

df<- load("Merged_data/df_merge_after_step41_A.RData")
#Write a variable for the distance between Erstzulassung and HU:


df$HU<-paste0(as.character(df$HU), "01") 
df$HU<- as.IDate(as.character(df$HU), format="%Y%m%d")
#df$lastDate<- as.IDate(as.character(df$lastDate), format="%Y%m%d")
#df$firstDate<- as.IDate(as.character(df$firstDate), format="%Y%m%d")


#Create a variable for interaction between type and categorie:
df$typeXcat<- paste(df$Typ, df$Kategorie, sep="x")
df$typeXcat<- gsub(" ", "", df$typeXcat)


#Calculate Market size:
#data<- data.table(df)

#marketSize<- function(data){
#  i=1
#  temp<- length(unique(
    
#    data$MobileID[ data$Typ==type.list 
#                   & data$Kategorie==data$Kategorie[i] 
#                   & abs(data$age-data$age[i])<=60 
#                   & abs(data$Kilometer-data$Kilometer[i])<=10000 
#                   & data$firstDate<=data$lastDate[i]
#                   & data$lastDate>= data$firstDate[i]
#                   ]
#  )
#  )
#print(paste0("Finished line", i))
#  return(temp)
#}

#type<- as.list(unique(df$Typ))

#df$MS<- rep(NA, rep(nrow(df))) 

marketSize<- function(typecat, df){
  data<- df[df$typeXcat==typecat,]
  print(paste0("Dataframe details: NROW:", nrow(data)))
  xy.list<- as.list(seq(from=1, to=nrow(data), by=1))
      marketsize.inner<-function(i, data){
      temp<- length(unique(
      data$MobileID[ abs(data$age-data$age[i])<=60 
                   & abs(data$Kilometer-data$Kilometer[i])<=10000 
                   & data$firstDate<=data$lastDate[i]
                   & data$lastDate>= data$firstDate[i]
                   ]
    )
    )
    print(paste0("Finished line", i))
    return(temp)
  }
  MS<- sapply(xy.list, marketsize.inner, data)
  #df$MS[df$Typ==type]<- MS
  #rm(data)
  data$MS<- MS
  write.table(data, paste0("H:\\MA\\Pkw\\generatedData\\MS\\marketSize", typecat, ".txt"), sep=";", row.names=F)
 return(print("Done!"))
}


typecat1=as.list(unique(df$typeXcat)[1:20])
lapply(typecat1, marketSize, df)
#hi<- do.call(rbind, lapply(typecat1, marketSize, df))


