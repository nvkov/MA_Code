#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
setwd("H:\\MA\\Pkw\\generatedData\\")


library("data.table")

df<- fread("H:\\MA\\Pkw\\generatedData\\finalMerge16408.txt", sep=";")
#Write a variable for the distance between Erstzulassung and HU:

df$Erstzulassung<-paste0(as.character(df$Erstzulassung), "/01") 
df$Erstzulassung<- as.IDate(as.character(df$Erstzulassung), format="%Y/%m/%d")

df$HU<-paste0(as.character(df$HU), "/01") 
df$HU<- as.IDate(as.character(df$HU), format="%Y/%m/%d")
df$lastDate<- as.IDate(as.character(df$lastDate), format="%Y%m%d")
df$firstDate<- as.IDate(as.character(df$firstDate), format="%Y%m%d")

df$age<- difftime(df$lastDate, df$Erstzulassung, units="days")

#It is important to calculate this after merging all datasets!
df$HUEZdiff<- difftime(df$HU, df$Erstzulassung, units="days")

#Calculate Market size:
#data<- data.table(df)

marketSize<- function(data){
  i=1
  temp<- length(unique(
    
    data$MobileID[ data$Typ==type.list 
                   & data$Kategorie==data$Kategorie[i] 
                   & abs(data$age-data$age[i])<=60 
                   & abs(data$Kilometer-data$Kilometer[i])<=10000 
                   & data$firstDate<=data$lastDate[i]
                   & data$lastDate>= data$firstDate[i]
                   ]
  )
  )
print(paste0("Finished line", i))
  return(temp)
}

type<- as.list(unique(df$Typ))

df$MS<- rep(NA, rep(nrow(df))) 

marketSize<- function(type, df){
  data<- df[df$Typ==type,]
  print(paste0("Dataframe details: NROW:", nrow(data)))
  xy.list<- as.list(seq(from=1, to=nrow(data), by=1))
      marketsize.inner<-function(i, data){
      temp<- length(unique(
      data$MobileID[ data$Kategorie==data$Kategorie[i] 
                   & abs(data$age-data$age[i])<=60 
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
 return(data)
}

hi<- do.call(rbind, lapply(type, marketSize, df))

type="SL 350"

#Split data.table according to type:
#setkey(df, "Typ")
#df = lapply(as.list(unique(df$Typ)), function(x)data.table(df[x]))


#for(i in(1:2215)){
#setkey(df[[i]], "Kategorie" )
#df[[i]] = lapply(as.list(unique(df[[i]]$Kategorie)), function(x)data.table(df[[i]][x]))
#}


#hi<- df[[1]][[1]]
#data<- df[[12]]

#hi<- rapply(sdt1, marketSize, how="list")
#hi<- lapply(data, sapply, marketSize)


#xy.list<- as.list(seq(from=1, to=nrow(df), by=1))
xy.list<- as.list(seq(from=1, to=nrow(hi), by=1))



marketsize<-sapply(xy.list, FUN=marketSize, data = hi)
marketsize<- as.numeric(as.character(marketsize))

df<- cbind(df, marketSize)


