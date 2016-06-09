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

expandRows <- function(dataset, count, count.is.col = TRUE) {
  if (!isTRUE(count.is.col)) {
    if (length(count) == 1) {
      dataset[rep(rownames(dataset), each = count), ]
    } else {
      if (length(count) != nrow(dataset)) {
        stop("Expand vector does not match number of rows in data.frame")
      }
      dataset[rep(rownames(dataset), count), ]
    }
  } else {
    dataset[rep(rownames(dataset), dataset[[count]]), 
            setdiff(names(dataset), names(dataset[count]))]
  }
}

#Subset for cars hidden new cars:
df_merge<- df_merge[df_merge$Kilometer>1000,]


df_leasing<- df_merge[df_merge$TOM==1,]


#Inspect vendor behavior:
View(df_merge[df_merge$Typ=="A170" & df_merge$valuePrice==2700 & df_merge$Erstzulassung=="2003-12-01" &df_merge$Farbe=="59",])
# Safe vendor_IDs from same group of vendors:
df_merge$vendor_ID[df_merge$Typ=="A170" & df_merge$valuePrice==2700 & df_merge$Erstzulassung=="2003-12-01" &df_merge$Farbe=="59"]

#Vendor_Group:
vendor_group<- c(611815, 450931, 795790, 468982, 455508, 458936, 470362, 494168, 471189, 458767)
#Inspect full behavior of the vendor group:
df_vendorG<- df_merge[df_merge$vendor_ID %in% vendor_group,]
#Obvious strategy: offer a car for one day from each vendor_ID. Order is shown above,
# make a pause of 2 weeks, place offer under the next ID. problem: all offers are place the exact number of times
# this might indicate that the car still wasn't sold even after the last placement






