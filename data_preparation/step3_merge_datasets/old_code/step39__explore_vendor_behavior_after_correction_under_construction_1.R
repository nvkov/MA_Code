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
load("Merged_data/df_merge_after_step37.RData")

###########################################################################

# Keep only relevant variables --------------------------------------------
irrelCols<- c("rows")
df_merge<- df_merge[, !irrelCols, with=F]


# Order according to first date of entry ----------------------------------
df_merge[order(df_merge$prices_firstDate)]
vendors<- df_merge
rm(df_merge)

# Step 1: Inspect for vendors that change car specs -----------------------
vendors<- vendors[,`:=`(lag_specs_change=diff(cars_lastChange, 1),
                        n_chnages_specs=.N, 
                        avg_TOM_specs= mean(TOM)),
                   by=.(car_ID, vendor_ID, Typ, Erstzulassung, valuePrice)]


# Step 2: Inspect for vendors that change car prices ----------------------
vendors<- vendors[,`:=`(lag_price_change=diff(prices_firstDate, 1),
                       n_changes_price=.N, 
                       avg_TOM_price=mean(TOM)),
                   by=.(car_ID, vendor_ID, Typ, Erstzulassung)]


# Step 3: Inspect for vendors that change car_ID --------------------------
vendors<- vendors[,`:=`(lag_carID_change=diff(prices_firstDate, 1),
                        n_changes_carID=.N),
                  by=.(vendor_ID, Typ, Erstzulassung, valuePrice)]


# Step 4: Inspect for vendors that have vendor_ID pools -------------------
vendors<- vendors[,`:=`(vendorID_pool=paste(unique(vendor_ID), collapse = ", "),
                        variance_vendor_pool=var(vendor_ID),
                        n_changes_vendor_pool=.N, 
                        n_unique_changes_vendor_pool=length(unique(vendor_ID))),
                  by=.(Typ, Erstzulassung, valuePrice, Schaltung, 
                       Leistung, Eigenschaften)]


# Step 5: Inspect for vendors that change several specs and car_ID---------
vendors<- vendors[,`:=`(similar_cars_diff_IDs=.N,
                        variance_km=var(Kilometer),
                        variance_price=var(valuePrice), 
                        pool_firstDate=paste(cars_lastChange, collapse=", ")),
                  by=.(Typ, Erstzulassung, Schaltung, 
                       Leistung, Eigenschaften, vendor_ID)]


# Step 6: Inspect for vendors with leasing offers or several classes--------
vendors<- vendors[,`:=`(leasing=.N, 
                        leasing_carIDs=paste(car_ID, collapse=", "), 
                        variance_category=var(as.numeric(as.character(Kategorie)))),
                        by=.(vendor_ID, Typ, Erstzulassung, Schaltung, 
                              Leistung, Eigenschaften, cars_lastChange)]



# Save vendor data --------------------------------------------------------

save(vendors, file="C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Vendors/step39_vendor_behavior.Rdata")







