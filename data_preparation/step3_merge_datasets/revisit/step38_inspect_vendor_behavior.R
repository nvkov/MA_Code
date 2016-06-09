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


#Function for expanding rows for leasing cars:


#Inspect vendor behavior:
View(df_merge[df_merge$Typ=="A170" & df_merge$valuePrice==2700 & df_merge$Erstzulassung=="2003-12-01" &df_merge$Farbe=="59",])
# Safe vendor_IDs from same group of vendors:
df_merge$vendor_ID[df_merge$Typ=="A170" & df_merge$valuePrice==2700 & df_merge$Erstzulassung=="2003-12-01" &df_merge$Farbe=="59"]

#Vendor_Groups:
vendor_group<- c(611815, 450931, 795790, 468982, 455508, 458936, 470362, 494168, 471189, 458767)
#Inspect full behavior of the vendor group:
df_vendorG<- df_merge[df_merge$vendor_ID %in% vendor_group,]
#Obvious strategy: offer a car for one day from each vendor_ID. Order is shown above,
# make a pause of 2 weeks, place offer under the next ID. problem: all offers are place the exact number of times
# this might indicate that the car still wasn't sold even after the last placement

#Inspect for vendor groups:
View(df_merge[df_merge$TOM==1 & df_merge$consecutives==1 & !df_merge$vendor_ID,])

df_merge_vendor_groups<- df_merge[df_merge$TOM==1 & df_merge$consecutives==1 & !df_merge$vendor_ID %in% vendor_group,]
df_merge_vendor_groups<- df_merge[,.(vendor_group=paste(vendor_ID, collapse=","), members=.N)
                                  ,by=.(valuePrice, Typ, Kategorie, Farbe, HU, Erstzulassung, 
                                        Emission, Kraftstoff, Leistung, Schaltung, Klimatisierung,
                                        Hubraum, Eigenschaften, Kilometer, TOM)]

vendor_groups<- df_merge_vendor_groups$vendor_group[df_merge_vendor_groups$members>3]
summary(unique(df_merge_vendor_groups$members[df_merge_vendor_groups$members>1]))

#vendor_group2

# Vendors that use consecutives:
View(df_merge[df_merge$consecutives>1,]) 

View(df_merge[df_merge$vendor_ID=="459034"])

#Example
View(df_merge[df_merge$Typ=="B200" & df_merge$Erstzulassung=="2007-11-01" & df_merge$Farbe=="51" & df_merge$vendor_ID=="451177",])

#


