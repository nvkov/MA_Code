# Cluster cars for the same vendor:

rm(list=ls())
library("data.table")
library("sets")
library("survival")
library("fpc")
library("cluster") 
library("magrittr")
library("dendextend")
library("dendextendRcpp")
library("rJava")
library("RWeka")


#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("df_full2007.RData")


#######################################################################
# vendor<- "7723851"
# relCols<- c("Erstzulassung", "valuePrice", "Kilometer", "car_ID", "Typ", 
#             "Eigenschaften", "Kraftstoff", "Schaltung", "vendor_ID")
# mydata<- df[vendor_ID==vendor, relCols, with=F]
# mydata<- mydata[Typ=="A150"]
# #mydata$Erstzulassung<- as.numeric(mydata$Erstzulassung)
# #mydata$Typ<- NULL
# mydata<- mydata[!duplicated(mydata$car_ID)]
# 

#######################################################################
# Calculate distances for cars from same vendor -----------------------
df<- df[,k:= floor(0.8*.N),
            by=.(Erstzulassung, Typ, Eigenschaften, Schaltung, vendor_ID)]

mydata<- df[df$k>2]

# Calculate distances for cars from same vendor -----------------------
mydata<- mydata[,cluster:=
                  as.data.frame(cutree(hclust(dist(cbind(valuePrice, Kilometer)), "ave"), k = 2))
                , by=.(Erstzulassung, Typ, Eigenschaften, Schaltung, Farbe)]

#Compare with xmeans:
mydata<- mydata[,xmeans:= xmeans(cbind(valuePrice, Kilometer))
                , by=.(Erstzulassung, Typ, Eigenschaften, Schaltung, Farbe)]


View(mydata)
#######################################################################
#This works!!! Keep :)

# find_clusters<- function(clusterVars){
#   mydata$uniqueID<- seq(1:nrow(mydata))
#     colnames(tree)[1]<- "cl"
#   mydata<- cbind(tree, mydata)
# return(mydata$cl)
# }
# 
# tree <- as.data.frame(cutree(hclust(dist(cbind(valuePrice, Kilometer)), "ave"), k = floor(0.8*nrow(mydata))))
# 


#one-liner

# Nice plot: try to integrate: plot tree ---------------------------------------------------------------

# # using piping to get the dend
# dend <- mydata[,-4] %>% dist %>% hclust %>% as.dendrogram
# 
# # plot + color the dend's branches before, based on 3 clusters:
# dend %>% color_branches(k=10) %>% plot(horiz=TRUE, main = "The dendextend package \n Gives extended functionality to R's dendrogram object")
# 
# # add horiz rect
# dend %>% rect.dendrogram(k=10,horiz=TRUE)
# 
# # add horiz (well, vertical) line:
# abline(v = heights_per_k.dendrogram(dend)["3"] + .6, lwd = 2, lty = 2, col = "blue")
