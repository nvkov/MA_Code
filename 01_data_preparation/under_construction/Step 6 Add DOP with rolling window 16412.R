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

originDate<- min(df$firstDate)
#Calculate Market size:

#First try at rolling window:

# Step 1: Keep only vars needed vor rolling window:
# MobileID, Kategorie, Typ, Kilometer, Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, Hubraum, valueProce, age

df<- df[, .(MobileID, Kategorie, Typ, Kilometer, Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung, Hubraum, valuePrice, age, firstDate, lastDate)]
df$firstMonth<- round(difftime(df$firstDate, originDate, units="days")/30)
df$lastMonth<- round(difftime(df$lastDate, originDate, units="days")/30)


