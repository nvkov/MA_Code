#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
setwd("H:\\MA\\Pkw\\generatedData\\IntermediaryDatasets\\")


library("data.table")
library("stringr")

df<- read.table("fahrzeuge_full_16405.txt", sep=" ", nrow=10000000)

names<- unique(df$Typ)
 length(names)
names<- as.character(names)
 names<- sort(names)
 fix(names)
 write.table(names, "H:\\MA\\Pkw\\generatedData\\validnames.txt")