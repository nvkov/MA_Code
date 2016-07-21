# Cluster cars for the same vendor:

rm(list=ls())
library("data.table")
library("sets")
library("survival")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("df_full2007.RData")


#######################################################################
vendor<- "7723851"
relCols<- c("Erstzulassung", "valuePrice", "Kilometer", "car_ID", "Typ")
mydata<- df[vendor_ID==vendor, relCols, with=F]
mydata<- mydata[Typ=="A150"]
mydata$Erstzulassung<- as.numeric(mydata$Erstzulassung)
mydata$Typ<- NULL
mydata<- mydata[!duplicated(mydata$car_ID)]


#######################################################################

# Calculate distances for cars from same vendor -----------------------
mydata<- mydata[, by=.(Erstzulassung, vendor_ID, Typ, Eigenschaften, Schaltung)]

clusterVars<- c("valuePrice", "Kilometer", "Erstzulassung")
mydata.c<- mydata[,clusterVars, with=F]

#######################################################################
#Experiment with other datasets:

mydata.c <- scale(mydata.c) # standardize variables
mydata.c <- na.omit(mydata.c) # listwise deletion of missing
# K-Means Clustering with 5 clusters
fit <- kmeans(mydata.c, 5)

# Cluster Plot against 1st 2 principal components

plotcluster(mydata.c, fit$cluster)  # your usual method 
dcmat <- discrcoord(mydata.c, fit$cluster)
#plot(dcmat$proj[,1], dcmat$proj[,2], pch="")   # scatter plot of 1st two dcs
text(dcmat$proj[,1], dcmat$proj[,2], labels=mydata$car_ID)  # label points with row index


library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)




# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=20) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")




labs = paste("sta_",1:50,sep="") #new labels
rownames(USArrests)<-labs #set new row names
hc <- hclust(dist(USArrests), "ave")

library(ggplot2)
library(ggdendro)

#convert cluster object to use with ggplot
dendr <- dendro_data(hc, type="rectangle") 

#your own labels (now rownames) are supplied in geom_text() and label=label
require(graphics)
labs = paste("sta_",1:50,sep="") #new labels
USArrests2<-USArrests #new data frame (just to keep original unchanged)
rownames(USArrests2)<-labs #set new row names
hc <- hclust(dist(USArrests2), "ave")
par(mar=c(3,1,1,5)) 
plot(as.dendrogram(hc),horiz=T)
