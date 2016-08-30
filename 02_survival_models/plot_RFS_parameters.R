#Plot RFS parameters:

rm(list=ls())
library("data.table")
library("sets")
library("survival")
library("stargazer")
library("beepr")
library("rpart")
library("partykit")
library("coin")
library("caret")
library("verification") # for Brier score
library("pec")
library("survAUC")
library("LTRCtrees")
library("peperr")
library("rms")
library("randomForestSRC")
library("party")
library("ipred")
library("survivalROC")
library("ggplot2")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/MA_Code/Results/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

load("rsf.results5000_ASeries.RData")

p<- ggplot(rfs.results, aes(as.factor(ntree), forecasts, fill=splitrule))
p + geom_boxplot() + coord_flip()
