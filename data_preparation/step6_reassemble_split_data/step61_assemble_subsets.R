#Remerge the split data

rm(list=ls())
library("data.table")
library("sets")
library("survival")



#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/step5/with_MS/"
final_data<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)

setwd(wd)


files.list<- list.files(pattern="new*")[1:250]

#load files and merge:
my.list<- lapply(files.list, function(x){
  load(file=x)
  get(ls()[ls()!="filename"])
})

names(my.list)<- files.list

#read all DTs separately:
#list2env(my.list,.GlobalEnv)

#Merge all in one file:
df<- do.call("rbind", my.list)

save(df, file=paste0(project_directory, final_data, "dataset_full.RData"))
