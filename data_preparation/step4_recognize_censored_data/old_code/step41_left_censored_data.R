#Find vendors with several vendor_IDs

#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<- "/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("Merged_data/df_merge_after_step37.RData")

df_merge<- df_merge[order(df_merge$car_ID),]
df_merge<- df_merge[order(df_merge$prices_firstDate),]

#Find bought cars:
df_merge<- df_merge[,`:=`( COUNT = .N , IDX = 1:.N ),
                    by=.(car_ID, Typ)]

df_merge$bought<- rep(0, nrow(df_merge))
df_merge$bought[df_merge$COUNT==df_merge$IDX]<- 1

df_merge$right_censored<- rep(1, nrow(df_merge))
df_merge$right_censored[df_merge$bought==0]<- 0
df_merge$right_censored[df_merge$prices_lastDate=="2012-12-18"]<-0

df_merge$newTOM<- as.numeric(df_merge$prices_lastDate-df_merge$prices_firstDate) +1

df_merge$age<- as.numeric(df_merge$cars_lastChange-df_merge$Erstzulassung)

# Subset for used cars (drop young cars - 7 months or low km count): 
df_merge[df_merge$age<210,]<- NULL
df_merge<-df_merge[df_merge$Kilometer>1000 &df_merge$age>210,]

df_merge_A<- df_merge[grep("A", df_merge$Typ),]


save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step41.RData" ))
save(df_merge_A, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step41_A.RData" ))

