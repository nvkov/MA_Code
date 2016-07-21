#Explore vendor behavior:

rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<- "/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("Merged_data/df_merge_after_step32.RData")

###########################################################################

# Keep only relevant variables --------------------------------------------
irrelCols<- c("rows")
df_merge<- df_merge[, irrelCols, with=F]


# Order according to first date of entry ----------------------------------
df_merge[order(df_merge$prices_firstDate)]


# Step 1: Inspect for vendors that change car specs -----------------------
vendors<- df_merge[,lag_specs_change:=diff(cars_lastChange, 1),
                   by=.(car_ID, vendor_ID, Typ, Erstzulassung, valuePrice)]


# Step 2: Inspect for vendors that change car prices ----------------------
vendors<- vendors[,lag_price_change:=diff(prices_firstDate, 1),
                   by=.(car_ID, vendor_ID, Typ, Erstzulassung)]


# Step 3: Inspect for vendors that change car_ID --------------------------
vendors<- vendors[,lag_carID_change:=diff(prices_firstDate, 1),
                  by=.(car_ID, Typ, Erstzulassung, valuePrice)]


# Step 4: Inspect for vendors that have vendor_ID pools -------------------
vendors<- vendors[,vendorID_pool:=paste(vendor_ID, collapse = ", "),
                  by=.(Typ, Erstzulassung, valuePrice, Schaltung, 
                       Leistung, Eigenschaften)]







