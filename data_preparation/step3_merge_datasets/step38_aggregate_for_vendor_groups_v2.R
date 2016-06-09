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
library("DescTools")
#source("https://bioconductor.org/biocLite.R")
#biocLite("IRanges")
library("IRanges")

#Read files with car specifications:
load("Merged_data/df_merge_after_step37.RData")

#======Experiment with overlaps:
df.mini<- df_merge[1:20,]

seeOverlaps<-function(start , stop){
ir<- IRanges(as.numeric(as.IDate(start)), as.numeric(as.IDate(stop)))
group<- subjectHits(findOverlaps(ir, reduce(ir)) )
return(group)
}
#================
vendor_group<- c(611815, 450931, 795790, 468982, 455508, 458936, 470362, 494168, 471189, 458767)

df_merge1<- df_merge[df_merge$vendor_ID %in% vendor_group,]

df_merge1<- df_merge1[ ,.(car_ID=(car_ID), vendor_ID=paste(vendor_ID, collapse=","),
                       cars_lastChange=cars_lastChange, cars_lastDate=cars_lastDate, 
                       Anzeigenanlage=Anzeigenanlage, leasing=max(leasing), 
                       leasing_change=max(leasing)-min(leasing), TOM=sum(TOM),
                       prices_firstDate=prices_firstDate, prices_lastDate=prices_lastDate,
                       group=seeOverlaps(Anzeigenanlage, prices_firstDate)) ,
                    
                      by=.(valuePrice, Typ, Kategorie, Farbe, HU, Erstzulassung, 
                         Emission, Kraftstoff, 
                         Leistung, Schaltung, Klimatisierung, Hubraum,
                         Eigenschaften, Kilometer, consecutives) ]

df_merge$realTOM<- difftime(df_merge$prices_lastDate, df_merge$prices_firstDate, units="days")
df_merge$merge_check<- df_merge$realTOM-df_merge$TOM

View(df_merge[as.numeric(df_merge$merge_check)<=-2,])

save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step38.RData" ))
A180<-df_merge[df_merge$Typ=="A180",]
save(A180, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step38_A180.RData" ))
